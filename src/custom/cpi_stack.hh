#ifndef __CPI_STACK_HH__
#define __CPI_STACK_HH__

#include "sim/ticked_object.hh"

/*
    Attaches to CPU and tracks execution of cpu (is issued or if not why not?)

    I think should also include interface for unit we attach this to, so can call into it to find route cause
    TODO for now simplify and dont do this. iocpu ticks every cycle so dont need to worry about logging when sleeping
*/

// abstract class (interface) implementing the lookup function required to lookup
class CPIStackInterace {

  public:

    // enum for things we want to track
    // need to have resulting lookup functions in the observed model
    // TODO if want to do with other cpus, should make children class based on this
    enum CPIEvents {
        Issued,
        Stallon_Load,
        Stallon_Frame,
        Stallon_Fetch,
        Stallon_INET_Pull,
        Stallon_ROB_Store,
        Stallon_ROB_Other,
        Stallon_ExeUnit,
        Stallon_DepOther,
        Stallon_DepNone,
        Stallon_MemBarrier,
        Stallon_Remem,
        Num_Events
    };

    const char *CPIEventStrings[CPIEvents::Num_Events] =
    {
        "Issued",
        "Stallon_Load",
        "Stallon_Frame",
        "Stallon_Fetch",
        "Stallon_INET_Pull",
        "Stallon_ROB_Store",
        "Stallon_ROB_Other",
        "Stallon_ExeUnit",
        "Stallon_DepOther",
        "Stallon_DepNone",
        "Stallon_MemBarrier",
        "Stallon_Remem",
    };

    // measure cpu for this cycle
    // implements priority order and calls the appropriate function in cpu to check
    // virtual CPIEvents MeasureCPU();

    // schedules on event queue
    // virtual void schedule_measure(Event &event, Tick when);
};


class CPIStack : public Named, public Clocked {
    public:
        // constructor
        // CPIStack(std::string base_name, ClockDomain& clk, CPIStackInterace& target);

        CPIStack(std::string base_name, ClockDomain& clk);

        // register stats for this
        void regStats();

        // informing what happened this cycle
        // void setIssuedThisCycle();

        void setEventThisCycle(CPIStackInterace::CPIEvents event);

    protected:

        // the function to run every cycle
        // void MeasureEvent();

    protected:

        // recording of different behaviors
        Stats::Vector2d _stack;

        // make sure we are recording exactly once per cycle
        Cycles _lastCycleRecorded;



        // TODO only needed if cpu goes to sleep

        // // schedule event to record
        // // to make sure goes after cpu execution schedule for cycle + 1tick
        // EventFunctionWrapper _measureEvent;

        // // target to measure
        // CPIStackInterace& _target;
        CPIStackInterace _dummy;

        // bool _issuedThisCycle;



};



#endif
