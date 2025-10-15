FUNCTION ZRFFOM_ITEM_INTERBANKING_ARG.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"     VALUE(I_DTAM100) LIKE  DTAM100 STRUCTURE  DTAM100
*"  EXPORTING
*"     VALUE(E_DTAM100) LIKE  DTAM100 STRUCTURE  DTAM100
*"  TABLES
*"      T_REGUP STRUCTURE  REGUP
*"      T_DTAM100V STRUCTURE  DTAM100V
*"----------------------------------------------------------------------
  CLASS CL_ABAP_CHAR_UTILITIES DEFINITION LOAD.

  DATA: v_bkont  TYPE LFBK-BKONT.

  DATA: V_AMOUNT TYPE P DECIMALS 2. "Amount
  DATA: R_AMOUNT(19) TYPE C.

  DATA: BEGIN OF ls_interbanking,
          TIPO(3)      TYPE C VALUE '*M*',
          COD(3)       TYPE N,
          TCTA(2)      TYPE N, "VALUE '01', "Modificado  21.11.2012
          NROCTA(17)   TYPE C,
          AMOUNT(17)   TYPE N,            "
          OBS(60)      TYPE C,            "Blancos
          TIPODOC(2)   TYPE C,            "Tipo Documento a cancelar
          NRODOC(12)   TYPE C,
          TIPO_OP(2)   TYPE C,
          NRO_OP(12)   TYPE C,            "Zeros
          CODCLI(12)   TYPE C,
          TIPORET(2)   TYPE C,
          TOTRET(12)   TYPE N,
          NRONC(12)    TYPE C,
          IMPNC(10)    TYPE N,
          CUIT(11)     TYPE C,
          CBU(22)      TYPE C,
          DELIM(29)    TYPE C,
*                      VALUE CL_ABAP_CHAR_UTILITIES=>cr_lf, "<CR/LF>
        END OF   ls_interbanking.

*----------------------------------------------------------------------*
* remove mt100 entries in dme file                                     *
*----------------------------------------------------------------------*

  CALL FUNCTION 'NO_MT100'
    TABLES
      T_DTAM100V = T_DTAM100V
    CHANGING
      C_DTAM100  = E_DTAM100.

*----------------------------------------------------------------------*
*  Mapping  I_REGUP,T_REGUP -> ls_interbanking                         *
*----------------------------------------------------------------------*

  MOVE i_reguh-zbnkl TO ls_interbanking-cod.
  MOVE i_reguh-zbnkn TO ls_interbanking-nrocta.

* Modificado 21.11.2012 - Tipo de Cuenta
  CLEAR v_bkont.
  SELECT SINGLE BKONT
    INTO v_bkont
    FROM LFBK
    WHERE LIFNR EQ i_reguh-lifnr
      AND banks EQ i_reguh-zbnks
      AND bankl EQ i_reguh-zbnkl
      AND bankn EQ i_reguh-zbnkn.
  CASE v_bkont.
    WHEN 'CC'.
    ls_interbanking-tcta = '01'.
    WHEN 'CA'.
    ls_interbanking-tcta = '02'.
    WHEN others.
    ls_interbanking-tcta = '01'.
  ENDCASE.
* Fin 21.11.2012

* Modificado en 22.04.2013 - BCI - Importe en Moneda local
* V_AMOUNT  = I_REGUH-RWBTR.
*---> 15/06/2023 - Migração S4 - JS
*    V_AMOUNT  = I_REGUH-RBETR.
   V_AMOUNT = CONV #( I_REGUH-RBETR ).
*<--- 15/06/2023 - Migração S4 - JS

* Fin 22.04.2013

  WRITE V_AMOUNT TO R_AMOUNT.

  TRANSLATE R_AMOUNT USING '. , '.
  CONDENSE R_AMOUNT NO-GAPS.

  WRITE R_AMOUNT TO ls_interbanking-AMOUNT
  RIGHT-JUSTIFIED.
  TRANSLATE ls_interbanking-AMOUNT USING ' 0'.

  MOVE i_reguh-stcd1 TO ls_interbanking-cuit.
  MOVE i_reguh-koinh TO ls_interbanking-cbu.


*----------------------------------------------------------------------*
* using of help fields in global memory of function group to calculate *
* required information, e.g. control sums for the transaction record   *
*----------------------------------------------------------------------*
  " g_helpfield_1 = ... .
  "  . . .
  " g_helpfield_n = ... .

*----------------------------------------------------------------------*
* fill exporting table t_dtam100v                                      *
*----------------------------------------------------------------------*

  MOVE  ls_interbanking   TO T_DTAM100V-VALUE.

  DESCRIBE FIELD ls_interbanking LENGTH T_DTAM100V-LENGTH
                                 IN CHARACTER MODE.
  APPEND T_DTAM100V.

*  E_DTAM100-XCRLF_SUPP             = 'X' .
* E_DTAM100-XAVIS_REQ              = SPACE .

ENDFUNCTION.
