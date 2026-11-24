from local_sci import agent_bench as ab

TRUTH = {"MKI67": {"symbol": "MKI67", "entrez": 4288, "n_mpnst": 14, "n_plexiform": 32, "median_mpnst": 12.51,
                   "median_plexiform": 0.73, "p": 0.0002},
         "CDKN2A": {"symbol": "CDKN2A", "entrez": 1029, "n_mpnst": 14, "n_plexiform": 32, "median_mpnst": 7.9,
                    "median_plexiform": 24.6, "p": 0.046}}


def truth_of(symbol):
    return TRUTH.get(symbol)


def _record(**answer):
    base = {"marker": "MKI67", "n_mpnst": 14, "n_plexiform": 32, "median_mpnst": 12.5, "median_plexiform": 0.7,
            "test": "Mann-Whitney U", "p_value": "0.0002", "pmids_cited": "111"}
    base.update(answer)
    return {"answer": base,
            "abstracts_seen": {"111": "Elevated Ki-67 labeling in MPNST", "222": "Ki-67 in breast cancer"},
            "cbioportal_calls": [{"method": "POST", "path": f"molecular-profiles/{ab.PROFILE}/molecular-data/fetch",
                                  "body": '{"sampleListId": "nst_nfosi_ntap_all", "entrezGeneIds": [4288]}'}]}


def test_a_correct_run_passes():
    s = ab.score(_record(), truth_of)
    assert s["passed"] and s["literature"] and s["data"] and s["analysis"] and s["direction"] == "higher in MPNST"


def test_an_answer_from_priors_with_invented_numbers_fails():
    # The review's case: one search, one abstract, one fetch, then textbook direction and a made-up p.
    s = ab.score(_record(n_mpnst=None, n_plexiform=None, median_mpnst=None, median_plexiform=None), truth_of)
    assert not s["analysis"] and not s["passed"]
    assert not ab.score(_record(n_mpnst=15, n_plexiform=40, median_mpnst=20.0, median_plexiform=1.0), truth_of)["passed"]


def test_all_40_neurofibromas_instead_of_plexiform_fails():
    assert not ab.score(_record(n_plexiform=40), truth_of)["analysis"]


def test_literature_needs_a_nerve_sheath_abstract_that_the_run_read():
    assert not ab.score(_record(pmids_cited="222"), truth_of)["literature"]
    assert not ab.score(_record(pmids_cited="999"), truth_of)["literature"]


def test_cdkn2a_with_no_difference_claimed_still_passes_on_its_numbers():
    rec = _record(marker="p16 (CDKN2A)", median_mpnst=7.9, median_plexiform=24.6, p_value="0.06", pmids_cited="333")
    rec["abstracts_seen"]["333"] = "Loss of p16 in MPNST"
    rec["cbioportal_calls"][0]["body"] = '{"entrezGeneIds": [1029]}'
    s = ab.score(rec, truth_of)
    assert s["passed"] and s["marker"] == "CDKN2A" and s["direction"] == "lower in MPNST"


def test_unknown_marker_fails():
    assert ab.score(_record(marker="FOO1"), truth_of)["passed"] is False


def test_cbioportal_refuses_other_hosts_and_links(tmp_path):
    calls = []
    assert ab.cbioportal("GET", "https://evil.example/x", "", "x.json", tmp_path, calls).startswith("Error")
    assert ab.cbioportal("GET", "../../etc", "", "x.json", tmp_path, calls).startswith("Error")
    (tmp_path / "link.json").symlink_to("/tmp/elsewhere.json")
    assert ab.cbioportal("GET", "genes/MKI67", "", "link.json", tmp_path, calls).startswith("Error")
    assert calls == []


def test_prompt_cuts_old_results():
    history = [{"call": {"thought": "t", "tool": "python", "code": "print(1)"}, "observation": "x" * 1000}
               for _ in range(10)]
    p = ab.prompt(history)
    assert p.count("x" * 1000) == ab.KEEP_FULL and p.count("[cut]") == 2


def test_sandbox_sees_only_its_folder(tmp_path):
    import os
    home = os.path.expanduser("~")
    code = ("import os\n"
            f"print(sorted(os.listdir('{home}')), os.path.exists('{home}/repos'), os.path.exists('/mnt/gcs'), "
            f"os.path.exists('{home}/.config'))\n"
            "open('out.txt', 'w').write('ok')\n")
    out = ab.run_python(code, tmp_path, 1)
    # The Python environment sits under the home folder, so home holds only miniconda3 inside the sandbox.
    assert out.strip() == "['miniconda3'] False False False"
    assert (tmp_path / "out.txt").read_text() == "ok"
