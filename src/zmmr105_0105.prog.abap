*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0105 .
*----------------------------------------------------------------------*

DATA: E_LFBK TYPE LFBK,
      E_BNKA TYPE BNKA.

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_UPDATE_FLAG INPUT.
  ZIB_CTE_DIST_TER-CK_MANUAL = ABAP_TRUE.
ENDMODULE.                 " SET_UPDATE_FLAG  INPUT


*&---------------------------------------------------------------------*
*&      Module  STATUS_0105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0105 OUTPUT.

*  LOOP AT SCREEN.
*    IF ZIB_CTE_DIST_TER-CK_FINALIZADO IS NOT INITIAL.
*      IF SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-EBELN' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-EBELP' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-MWSKZ' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-BELNR' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-GJAHR' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-PESO_ORIGEM'  OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-PESO_CHEGADA' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-DT_CHEGADA' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-ZDT_MOV' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-ZDT_VENCTO' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-ZVALOR_PEDAGIO' OR
*         SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-ZBVTYP'.
*        SCREEN-INPUT     = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

  IF ZIB_CTE_DIST_TER-ZBVTYP IS NOT INITIAL.
    CLEAR: WA_INFO_FORNE.

    CALL METHOD OBJ_CTE->BUSCA_BANCO_PARCEIRO
      IMPORTING
        E_LFBK     = E_LFBK
        E_BNKA     = E_BNKA
      CHANGING
        P_CTE      = ZIB_CTE_DIST_TER
      EXCEPTIONS
        ERRO_BANCO = 1
        OTHERS     = 2.

    IF SY-SUBRC IS INITIAL.
      WA_INFO_FORNE-BVTYP = E_LFBK-BVTYP.
      WA_INFO_FORNE-BANKL = E_BNKA-BANKL(3).
      WA_INFO_FORNE-BANKA = E_BNKA-BANKA.
      WA_INFO_FORNE-BANKN = E_LFBK-BANKN.

      IF NOT E_LFBK-BKONT IS INITIAL.
        CONCATENATE E_LFBK-BANKL+4(11) '-' E_LFBK-BKONT INTO WA_INFO_FORNE-AGENC.
      ELSE.
        WA_INFO_FORNE-AGENC = E_LFBK-BANKL+4(11).
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR: WA_INFO_FORNE.
  ENDIF.

ENDMODULE.                 " STATUS_0105  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG_PESO  INPUT
*&---------------------------------------------------------------------*
MODULE SET_UPDATE_FLAG_PESO INPUT.
  ZIB_CTE_DIST_TER-CK_PESO_CHEGADA = ABAP_TRUE.
ENDMODULE.                 " SET_UPDATE_FLAG_PESO  INPUT

*&---------------------------------------------------------------------*
*&      Module  HLP_BVTYP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HLP_BVTYP INPUT.

  CALL METHOD OBJ_CTE->BUSCA_BANCO_PARCEIRO
    IMPORTING
      E_LFBK     = E_LFBK
      E_BNKA     = E_BNKA
    CHANGING
      P_CTE      = ZIB_CTE_DIST_TER
    EXCEPTIONS
      ERRO_BANCO = 1
      OTHERS     = 2.

ENDMODULE.                 " HLP_BVTYP  INPUT

*&---------------------------------------------------------------------*
*&      Module  HLP_MWSKZ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HLP_MWSKZ INPUT.

  TYPES:
     BEGIN OF TY_HELP,
       MWSKZ TYPE MWSKZ,
       TEXT1 TYPE TEXT1_007S,
     END OF TY_HELP.

  DATA: T_DYNPFIELDS TYPE STANDARD TABLE OF DYNPREAD INITIAL SIZE 1 WITH HEADER LINE,
        TI_HELP      TYPE STANDARD TABLE OF TY_HELP INITIAL SIZE 0 WITH HEADER LINE,
        T_RET        TYPE TABLE OF DDSHRETVAL,
        TI_T007A     TYPE TABLE OF T007A INITIAL SIZE 0 WITH HEADER LINE.

  DATA: ST_RET TYPE DDSHRETVAL.

  SELECT * INTO TABLE TI_T007A
    FROM T007A
   WHERE KALSM EQ 'TAXBRA'.

  CHECK NOT TI_T007A[] IS INITIAL.

  SELECT MWSKZ TEXT1
    INTO TABLE TI_HELP
    FROM T007S
    FOR ALL ENTRIES IN TI_T007A
   WHERE MWSKZ EQ TI_T007A-MWSKZ
     AND KALSM EQ TI_T007A-KALSM
     AND SPRAS EQ SY-LANGU.

  SORT TI_HELP BY MWSKZ.
CHECK NOT TI_HELP[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD   = 'MWSKZ'
      DYNPPROG   = SY-REPID
      DYNPNR     = SY-DYNNR
      VALUE_ORG  = 'S'
    TABLES
      VALUE_TAB  = TI_HELP[]
      RETURN_TAB = T_RET.

  READ TABLE T_RET INTO ST_RET INDEX 1.
  CHECK SY-SUBRC IS INITIAL.

  READ TABLE TI_HELP WITH KEY MWSKZ = ST_RET-FIELDVAL BINARY SEARCH.

  MOVE: 'ZIB_CTE_DIST_TER-MWSKZ' TO T_DYNPFIELDS-FIELDNAME,
        ST_RET-FIELDVAL          TO T_DYNPFIELDS-FIELDVALUE.
  APPEND T_DYNPFIELDS.

*  MOVE: 'TX_IVA'        TO  T_DYNPFIELDS-FIELDNAME,
*        TI_HELP-TEXT1   TO  T_DYNPFIELDS-FIELDVALUE.
*  APPEND T_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME     = SY-REPID
      DYNUMB     = SY-DYNNR
    TABLES
      DYNPFIELDS = T_DYNPFIELDS.

ENDMODULE.                 " HLP_MWSKZ  INPUT
