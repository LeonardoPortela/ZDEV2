FUNCTION Z_LES_RETENCAO_AQUA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_BRANCH) TYPE  J_1BBRANC_
*"     REFERENCE(I_LIFNR) TYPE  LIFNR
*"     REFERENCE(I_PESO) TYPE  BRGEW
*"     REFERENCE(I_PESO_FISCAL) TYPE  BRGEW OPTIONAL
*"     REFERENCE(I_PESO_RET_DESC) TYPE  BRGEW OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_RETIDO) TYPE  C
*"     REFERENCE(E_PERC_RETENCAO) TYPE  ZPERC_RET
*"     REFERENCE(E_PESO_RETIDO) TYPE  BRGEW
*"     REFERENCE(E_PESO_LIQUIDO) TYPE  BRGEW
*"----------------------------------------------------------------------

* Work Areas and Internal Tables ------------------------------------*
  DATA: WL_ZSDT0120 TYPE ZSDT0120,
        WL_ZLEST0152 TYPE ZLEST0152,
        IT_LFA1     TYPE TABLE OF LFA1,
        WL_LFA1     TYPE LFA1.

* Variables ---------------------------------------------------------*
  DATA: STCD_STR      TYPE C LENGTH 9,
        STCD_CONC     TYPE C LENGTH 4,
        VL_LIFNR_RAIZ TYPE LFA1-LIFNR,
        VL_TABIX      TYPE SY-TABIX,
        VL_PESO_RETIDO TYPE P DECIMALS 0,
        VL_VINC_FISCAL TYPE C.

  CLEAR: E_RETIDO,  "Parâmetro indicador de aplicação de retenção no Peso informado
         E_PERC_RETENCAO,
         E_PESO_RETIDO,
         E_PESO_LIQUIDO,
         VL_LIFNR_RAIZ,
         VL_VINC_FISCAL.

  CHECK ( I_BUKRS       IS NOT INITIAL ) AND
        ( I_BRANCH      IS NOT INITIAL ) AND
        ( I_LIFNR       IS NOT INITIAL ) AND
        ( I_PESO_FISCAL IS NOT INITIAL ) AND
        ( I_PESO   > 0                 ).

  SELECT SINGLE *
    FROM LFA1 INTO WL_LFA1
   WHERE LIFNR EQ I_LIFNR.

  CHECK SY-SUBRC = 0.

  CALL FUNCTION 'ZLES_PARAM_FORN_AQUA'
    EXPORTING
      I_BUKRS_ROM        = I_BUKRS
      I_BRANCH_ROM       = I_BRANCH
      I_LIFNR            = I_LIFNR
    IMPORTING
      E_LIFNR_RAIZ       = VL_LIFNR_RAIZ
      E_ZLEST0152        = WL_ZLEST0152.

  "Atribuição do Peso a ser dispobilizado para vinculação.

  "Não pode vincular quantidade superior ao peso fiscal.
  IF ( I_PESO > I_PESO_FISCAL ) OR ( WL_ZLEST0152-PESO_UTIL_FISCAL IS NOT INITIAL ).
    E_PESO_LIQUIDO  = I_PESO_FISCAL.
  ELSE.
    E_PESO_LIQUIDO  = I_PESO.
  ENDIF.

  IF I_PESO_RET_DESC IS NOT INITIAL.
    SUBTRACT I_PESO_RET_DESC FROM E_PESO_LIQUIDO.
  ENDIF.

  "Busca de Parâmetro Retenção
  CLEAR: WL_ZSDT0120.
  SELECT SINGLE *
    FROM ZSDT0120 INTO WL_ZSDT0120
   WHERE BUKRS  = I_BUKRS
     AND BRANCH = I_BRANCH
     AND LIFNR  = I_LIFNR.

  IF ( SY-SUBRC NE 0 ) AND ( VL_LIFNR_RAIZ IS NOT INITIAL ).
    SELECT SINGLE *
      FROM ZSDT0120 INTO WL_ZSDT0120
     WHERE BUKRS  = I_BUKRS
       AND BRANCH = I_BRANCH
       AND LIFNR  = VL_LIFNR_RAIZ.
  ENDIF.

  IF ( WL_ZSDT0120 IS INITIAL ).
    WL_ZSDT0120-PERC_RET = '0.25'.
  ELSEIF ( WL_ZSDT0120 IS NOT INITIAL ) AND ( WL_ZSDT0120-PERC_RET = 0 ) .
    E_PERC_RETENCAO = 0.
    E_PESO_RETIDO   = 0.
    "E_PESO_LIQUIDO  = I_PESO.
    RETURN.
  ENDIF.

  CHECK ( WL_ZSDT0120-PERC_RET > 0   ).

  E_RETIDO        = 'X'.
  E_PERC_RETENCAO = WL_ZSDT0120-PERC_RET.
  VL_PESO_RETIDO  = ( I_PESO * WL_ZSDT0120-PERC_RET ) / 100.
  E_PESO_RETIDO   = VL_PESO_RETIDO.
  "E_PESO_LIQUIDO  = I_PESO - E_PESO_RETIDO.

ENDFUNCTION.
