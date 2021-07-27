#include "custom/cpi_stack.hh"

CPIStack::CPIStack(std::string base_name, ClockDomain& clk /*, CPIStackInterace& target*/) :
    Named(base_name + ".cpi_stack"),
    Clocked(clk),
    _lastCycleRecorded(0)
    /*_measureEvent([this] { MeasureEvent(); }, name()),
    _target(target),
    _issuedThisCycle(false)*/
{
    // _target.schedule_measure(_measureEvent, curCycle() + 1);
}

void
CPIStack::setEventThisCycle(CPIStackInterace::CPIEvents event) {
    // make sure recording EXACTLY once per cycle
    // make exception for first record
    assert(_lastCycleRecorded == curCycle() - 1 ||
        _lastCycleRecorded == 0);

    _stack[0][event]++;
}


// dont need this stuff if cpu ticks every cycle!
/*
void
CPIStack::MeasureEvent() {
    // make sure recording EXACTLY once per cycle
    // make exception for first record
    assert(_lastCycleRecorded == curCycle() - 1);

    CPIStackInterace::CPIEvents event;
    if (_issuedThisCycle)
        event = CPIStackInterace::CPIEvents::Issued;
    else
        event = _target.MeasureCPU();
    _stack[0][event]++;
    _lastCycleRecorded = curCycle();

    _issuedThisCycle = false;

    _target.schedule_measure(_measureEvent, curTick() + cyclesToTicks(Cycles(1)));
}

void
CPIStack::setIssuedThisCycle() {
    _issuedThisCycle = true;
}
*/

void
CPIStack::regStats() {
    _stack
        .init(1, CPIStackInterace::CPIEvents::Num_Events)
        .name(name() + ".cpi_stack")
        .desc("number of cycles for event")
        .flags(Stats::total | Stats::pdf | Stats::dist);
    _stack.ysubnames(_dummy.CPIEventStrings);
}

