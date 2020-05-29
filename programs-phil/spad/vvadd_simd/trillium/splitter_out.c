void my_kernel(int ReservedKeywords.MASK) {
	#ifdef SCALAR
VECTOR_EPOCH(ReservedKeywords.MASK);
#elif defined VECTOR
;
	#ifdef SCALAR
DEVEC(ReservedKeywords.DEVEC_ARG);
asm volatile(fence);
asm(scalar return);
#elif defined VECTOR
asm(vector return);
return;;
}