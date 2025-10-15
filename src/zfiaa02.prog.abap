*&----------------------------------------------------------------&*
*& Developer Enio Jesus
*& Transaction ZAA12
*& Controle de Frota
*&----------------------------------------------------------------&*

*&----------------------------------------------------------------&*
INCLUDE ZFIAA02_TOP      .
INCLUDE ZFIAA02_CLASS    .
INCLUDE ZFIAA02_FORMS    .
*&----------------------------------------------------------------&*
INITIALIZATION.
  DATA: R_SELECIONA_DADOS TYPE REF TO LCL_SELECIONA_DADOS,
        R_TIPO_OPERACAO   TYPE REF TO LCL_TIPO_OPERACAO,
        R_UTILS           TYPE REF TO ZUTILS.
*&----------------------------------------------------------------&*
  INCLUDE ZFIAA02_PBO_0100.
  INCLUDE ZFIAA02_PAI_0100.
*&----------------------------------------------------------------&*
  INCLUDE ZFIAA02_PBO_0110.
  INCLUDE ZFIAA02_PAI_0110.
*&----------------------------------------------------------------&*
  INCLUDE ZFIAA02_PBO_0140.
  INCLUDE ZFIAA02_PAI_0140.
*&----------------------------------------------------------------&*
  INCLUDE ZFIAA02_PBO_0160.
  INCLUDE ZFIAA02_PAI_0160.
*&----------------------------------------------------------------&*
  INCLUDE ZFIAA02_PBO_0200.
  INCLUDE ZFIAA02_PAI_0200.
*&----------------------------------------------------------------&*
  INCLUDE ZFIAA02_PBO_0300.
  INCLUDE ZFIAA02_PAI_0300.
*&----------------------------------------------------------------&*
