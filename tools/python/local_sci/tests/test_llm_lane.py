from local_sci.llm_lane import NineBExperimenter, _setting_schema, narrate, pick, propose

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


def test_propose_lists_the_no_change_setting_as_tried():
    client = StubClient({"settings": []})
    propose(client, "q", "m", SPACE, ["lo < hi"], "higher", [], k=3, budget_left=10, seed=1,
            baseline={"name": "50-450", "value": 1.0})
    assert "not allowed again (written lo-hi): 50-450." in client.calls[0]["user"]
    assert "The no-change setting 50-450 gives 1.0" in client.calls[0]["user"]


def test_propose_shows_setting_labels_beside_their_names():
    client = StubClient({"settings": []})
    history = [{"round": 1, "setting": {"name": "17", "target": 17}, "reason": "r",
                "row": {"median": 0.5, "ci_low": 0.1, "ci_high": 0.9}}]
    propose(client, "q", "m", {"target": (0, 45, 1)}, [], "higher", history, k=3, budget_left=10, seed=1,
            baseline={"name": "0", "value": 0.0}, labels={"0": "All cell types", "17": "Kidney-Ep"})
    user = client.calls[0]["user"]
    assert "1 | 17 | Kidney-Ep | 0.500 | 0.100 to 0.900 | r" in user
    assert "0 (All cell types), 17 (Kidney-Ep)" in user


def test_picks_given_by_label_come_back_as_setting_names():
    client = StubClient({"picks": [{"name": "Kidney-Ep", "reason": "r"}, {"name": "17", "reason": "r"},
                                   {"name": "21 (Lung family)", "reason": "r"}]})
    exp = NineBExperimenter(client, "q", "m", {"target": (0, 45, 1)}, [], "higher", 1,
                            labels={"17": "Kidney-Ep", "21": "Lung family"})
    picks, _ = exp.pick([])
    assert [p["name"] for p in picks] == ["17", "17", "21"]


def test_pick_and_narrate_return_fields():
    assert pick(StubClient({"picks": [{"name": "60-170", "reason": "r"}]}), "q", "m", SPACE, [], "higher", [], seed=1)[0][0]["name"] == "60-170"
    assert narrate(StubClient({"text": "I tried short windows first."}), "q", [], seed=1)[0].startswith("I tried")
