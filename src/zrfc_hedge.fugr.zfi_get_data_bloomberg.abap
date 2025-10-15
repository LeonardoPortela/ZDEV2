FUNCTION ZFI_GET_DATA_BLOOMBERG.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZDE_DATA_BLOOMBERG_OUT
*"----------------------------------------------------------------------


   RANGES: LRA_DATA_REGISTRO FOR ZFIT0087-DATA_REGISTRO.

   CLEAR: T_SAIDA[].

   CHECK I_DATA_INI IS NOT INITIAL AND
         I_DATA_FIM IS NOT INITIAL.

   APPEND VALUE #( SIGN = 'I'  OPTION = 'BT' LOW = I_DATA_INI HIGH = I_DATA_FIM ) TO LRA_DATA_REGISTRO.

   SELECT *
     FROM ZFIT0083 INTO TABLE @DATA(LIT_ZFIT0083)
    WHERE date_period_1 IN @LRA_DATA_REGISTRO.

   CHECK LIT_ZFIT0083[] IS NOT INITIAL.

*--------------------------------------------------------------------------------------*
*  Montar Saida
*--------------------------------------------------------------------------------------*
  LOOP AT LIT_ZFIT0083 ASSIGNING FIELD-SYMBOL(<FS_ZFIT0083>).
    APPEND INITIAL LINE TO T_SAIDA ASSIGNING FIELD-SYMBOL(<FS_SAIDA>).

    MOVE-CORRESPONDING <FS_ZFIT0083> TO <FS_SAIDA>.

  ENDLOOP.


ENDFUNCTION.
