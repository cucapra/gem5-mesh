/* Author : Shunning Jiang
   Date   : Oct 23, 2018 */
SECTIONS
{
  . = 0x10000000;
  .spm :
  {
    *(.spm)
  }
}
INSERT AFTER .bss

