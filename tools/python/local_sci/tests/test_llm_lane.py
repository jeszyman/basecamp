from local_sci.llm_lane import _setting_schema, narrate, pick, propose

SPACE = {"lo": (50, 300, 5), "hi": (70, 450, 5)}


class StubClient:
    def __init__(self, reply):
        self.reply, self.calls = reply, []

    def chat_json(self, system, user, schema, **options):
        self.calls.append({"system": system, "user": user, "schema": schema, "options": options})
        return self.reply, {}


def test_setting_schema_bounds_and_count():
    schema = _setting_schema(SPACE, 4)
    items = schema["properties"]["settings"]
    assert items["maxItems"] == 4
    assert items["items"]["properties"]["lo"] == {"type": "integer", "minimum": 50, "maximum": 300}
    assert "reason" in items["items"]["required"]


def test_propose_truncates_to_budget():
    reply = {"settings": [{"lo": 60 + 5 * i, "hi": 170, "reason": "r"} for i in range(5)]}
    client = StubClient(reply)
    settings, raw = propose(client, "q", "m", SPACE, ["lo < hi"], "higher", [], k=5, budget_left=2, seed=1)
    assert len(settings) == 2
    assert client.calls[0]["schema"]["properties"]["settings"]["maxItems"] == 2
    assert "Settings left in the budget: 2" in client.calls[0]["user"]


def test_propose_lists_settings_already_tried():
    row = {"median": 1.2, "ci_low": 1.0, "ci_high": 1.4}
    history = [{"round": 1, "setting": {"name": "100-170", "lo": 100, "hi": 170}, "reason": "r", "row": row}]
    client = StubClient({"settings": []})
    propose(client, "q", "m", SPACE, ["lo < hi"], "higher", history, k=3, budget_left=10, seed=1)
    assert "not allowed again (written lo-hi): 100-170." in client.calls[0]["user"]


def test_pick_and_narrate_return_fields():
    assert pick(StubClient({"picks": [{"name": "60-170", "reason": "r"}]}), "q", "m", SPACE, [], "higher", [], seed=1)[0][0]["name"] == "60-170"
    assert narrate(StubClient({"text": "I tried short windows first."}), "q", [], seed=1)[0].startswith("I tried")
