#ifndef __CUSTOM_CONFIG_COUNTER_HH__
#define __CUSTOM_CONFIG_COUNTER_HH__

/*
 * Keep track of the number of ops completed
 */ 

class TimingSimpleCPU;

class ConfigCounter {
  protected:
    int requiredCount;
    int currentCount;
    
    TimingSimpleCPU *cpu;
    
  public:
    // set the required count
    void setCount(int count);
    
    // increment the count on the next cycle
    void incrCount();
    
    // check if we have completed the count
    bool isComplete();
    
    ConfigCounter(TimingSimpleCPU *cpu) : requiredCount(0), 
      currentCount(0), cpu(cpu) {}
  
};









#endif
