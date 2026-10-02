from eth_consensus_specs.test.context import (
    single_phase,
    spec_test,
    with_config_overrides,
    with_phases,
)
from eth_consensus_specs.test.helpers.constants import HEZE, UINT64_MAX


def check_slot_boundaries(spec, genesis_time_ms, expected_times):
    for slot, start_ms, duration_ms in expected_times:
        slot = spec.Slot(slot)
        start_ms = spec.Uint64(start_ms)
        assert spec.compute_time_at_slot_ms(genesis_time_ms, slot) == start_ms
        assert spec.compute_slot_at_time_ms(genesis_time_ms, start_ms) == slot
        assert spec.compute_slot_at_time_ms(genesis_time_ms, start_ms + duration_ms - 1) == slot
        assert spec.compute_slot_at_time_ms(genesis_time_ms, start_ms + duration_ms) == slot + 1
        if slot > 0:
            assert spec.compute_slot_at_time_ms(genesis_time_ms, start_ms - 1) == slot - 1


@with_phases([HEZE])
@spec_test
@with_config_overrides(
    {"HEZE_FORK_EPOCH": 2, "SLOT_DURATION_MS": 12000, "SLOT_DURATION_MS_HEZE": 10000}
)
@single_phase
def test_slot_schedule_fork_boundary(spec):
    genesis_time_ms = spec.Uint64(123000)
    fork_slot = 2 * spec.SLOTS_PER_EPOCH
    fork_time_ms = genesis_time_ms + fork_slot * 12000

    assert spec.get_slot_duration_ms(spec.Epoch(0)) == 12000
    assert spec.get_slot_duration_ms(spec.Epoch(1)) == 12000
    assert spec.get_slot_duration_ms(spec.Epoch(2)) == 10000
    assert spec.get_slot_duration_ms(spec.Epoch(3)) == 10000
    check_slot_boundaries(
        spec,
        genesis_time_ms,
        [
            (0, genesis_time_ms, 12000),
            (fork_slot - 1, fork_time_ms - 12000, 12000),
            (fork_slot, fork_time_ms, 10000),
            (fork_slot + 1, fork_time_ms + 10000, 10000),
            (fork_slot + 100, fork_time_ms + 1000000, 10000),
        ],
    )
    assert (
        spec.compute_time_at_slot(spec.Uint64(123), spec.Slot(fork_slot + 1))
        == (fork_time_ms + 10000) // 1000
    )
    assert spec.compute_slot_at_time(spec.Uint64(123), (fork_time_ms + 10000) // 1000) == (
        fork_slot + 1
    )


@with_phases([HEZE])
@spec_test
@with_config_overrides(
    {"HEZE_FORK_EPOCH": 0, "SLOT_DURATION_MS": 12000, "SLOT_DURATION_MS_HEZE": 10000}
)
@single_phase
def test_slot_schedule_fork_at_genesis(spec):
    assert spec.get_slot_duration_ms(spec.GENESIS_EPOCH) == 10000
    check_slot_boundaries(
        spec,
        spec.Uint64(123000),
        [(0, 123000, 10000), (1, 133000, 10000), (100, 1123000, 10000)],
    )


@with_phases([HEZE])
@spec_test
@with_config_overrides(
    {
        "HEZE_FORK_EPOCH": UINT64_MAX,
        "SLOT_DURATION_MS": 12000,
        "SLOT_DURATION_MS_HEZE": 10000,
    }
)
@single_phase
def test_slot_schedule_unscheduled_fork(spec):
    genesis_time_ms = spec.Uint64(123000)
    slot = (UINT64_MAX - genesis_time_ms) // 12000
    assert spec.get_slot_duration_ms(spec.compute_epoch_at_slot(spec.Slot(slot))) == 12000
    assert spec.compute_time_at_slot_ms(genesis_time_ms, spec.Slot(slot)) == (
        genesis_time_ms + slot * 12000
    )
    assert spec.compute_slot_at_time_ms(genesis_time_ms, spec.Uint64(UINT64_MAX)) == slot
    check_slot_boundaries(spec, genesis_time_ms, [(0, 123000, 12000), (1, 135000, 12000)])


@with_phases([HEZE])
@spec_test
@with_config_overrides(
    {"HEZE_FORK_EPOCH": 1, "SLOT_DURATION_MS": 10000, "SLOT_DURATION_MS_HEZE": 12000}
)
@single_phase
def test_slot_schedule_longer_slots(spec):
    fork_slot = spec.SLOTS_PER_EPOCH
    fork_time_ms = fork_slot * 10000
    check_slot_boundaries(
        spec,
        spec.Uint64(0),
        [
            (fork_slot - 1, fork_time_ms - 10000, 10000),
            (fork_slot, fork_time_ms, 12000),
            (fork_slot + 1, fork_time_ms + 12000, 12000),
        ],
    )


@with_phases([HEZE])
@spec_test
@with_config_overrides({"HEZE_FORK_EPOCH": 2})
@single_phase
def test_slot_schedule_unchanged_duration(spec):
    duration_ms = spec.config.SLOT_DURATION_MS
    assert duration_ms == spec.config.SLOT_DURATION_MS_HEZE
    genesis_time_ms = spec.Uint64(123000)
    check_slot_boundaries(
        spec,
        genesis_time_ms,
        [
            (slot, genesis_time_ms + slot * duration_ms, duration_ms)
            for slot in range(4 * spec.SLOTS_PER_EPOCH)
        ],
    )


@with_phases([HEZE])
@spec_test
@with_config_overrides(
    {"HEZE_FORK_EPOCH": 2, "SLOT_DURATION_MS": 12000, "SLOT_DURATION_MS_HEZE": 10000}
)
@single_phase
def test_slot_schedule_config_updates(spec):
    genesis_time_ms = spec.Uint64(0)
    slot = spec.Slot(2 * spec.SLOTS_PER_EPOCH + 1)
    assert spec.compute_time_at_slot_ms(genesis_time_ms, slot) == (
        2 * spec.SLOTS_PER_EPOCH * 12000 + 10000
    )

    spec.config = spec.config._replace(
        HEZE_FORK_EPOCH=spec.Epoch(1),
        SLOT_DURATION_MS=spec.Uint64(8000),
        SLOT_DURATION_MS_HEZE=spec.Uint64(6000),
    )
    expected_time_ms = spec.SLOTS_PER_EPOCH * 8000 + (spec.SLOTS_PER_EPOCH + 1) * 6000
    assert spec.compute_time_at_slot_ms(genesis_time_ms, slot) == expected_time_ms
    assert spec.compute_slot_at_time_ms(genesis_time_ms, expected_time_ms) == slot
    assert spec.get_slot_duration_ms(spec.Epoch(0)) == 8000
    assert spec.get_slot_duration_ms(spec.Epoch(1)) == 6000


@with_phases([HEZE])
@spec_test
@with_config_overrides({"SLOT_DURATION_MS": 12000, "SLOT_DURATION_MS_HEZE": 10000})
@single_phase
def test_slot_schedule_heze_deadlines(spec):
    assert spec.get_slot_component_duration_ms(spec.Uint64(2500)) == 2500
    assert spec.get_slot_component_duration_ms(spec.BASIS_POINTS) == 10000
    assert spec.get_attestation_due_ms() == spec.config.ATTESTATION_DUE_BPS_GLOAS
    assert spec.get_inclusion_list_due_ms() == spec.config.INCLUSION_LIST_DUE_BPS
