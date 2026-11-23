from local_sci.bench import SPACE, draw_plants


def test_plants_lie_on_the_grid_inside_the_space():
    plants = draw_plants()
    assert len(plants) == 6
    for lo, hi in plants.values():
        assert SPACE["lo"][0] <= lo <= SPACE["lo"][1] and SPACE["hi"][0] <= hi <= SPACE["hi"][1]
        assert lo % 5 == 0 and hi % 5 == 0 and hi - lo >= 40


def test_plants_are_fixed_by_the_seed():
    assert draw_plants() == draw_plants()
