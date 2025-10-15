*&---------------------------------------------------------------------*
*&  Include           ZPM_VIEW_10_SAAFF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SAVE .
*
  LOOP AT T_VIEW.
    MOVE-CORRESPONDING T_VIEW TO ZTPM_VEI_OP_SAAF.
    MODIFY ZTPM_VEI_OP_SAAF.
    IF NOT SY-SUBRC IS INITIAL.
*      MESSAGE E000(ZPPM001) WITH 'Registro duplicado'.
    ENDIF.
  ENDLOOP.
*
  CLEAR: V_TYPBZ, V_CONSUMO, V_DESVIO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ELIMINA .
  LOOP AT T_VIEW WHERE MARC = ABAP_TRUE.
    DELETE T_VIEW.
    DELETE FROM ZTPM_VEI_OP_SAAF
           WHERE CODE  = T_VIEW-CODE
             AND EQART = T_VIEW-EQART
             AND TYPBZ = T_VIEW-TYPBZ.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERE .
*
  READ TABLE T_VIEW WITH KEY CODE = V_CODE
          EQART = V_EQART
          TYPBZ = V_TYPBZ.
  IF SY-SUBRC IS INITIAL.
    MESSAGE S000(ZPPM001) DISPLAY LIKE 'E'
      WITH 'Registro duplicado'.
  ELSEIF V_TYPBZ IS INITIAL.
    MESSAGE S000(ZPPM001) DISPLAY LIKE 'E'
         WITH 'Modelo não cadastrado'.
  ELSEIF V_EQART IS INITIAL.
    MESSAGE S000(ZPPM001) DISPLAY LIKE 'E'
         WITH 'Classe não cadastrada'.
  ELSEIF V_CODE IS INITIAL.
    MESSAGE S000(ZPPM001) DISPLAY LIKE 'E'
         WITH 'Operação não cadastrada'.
  ELSE.
    T_VIEW-CODE     = V_CODE.
    T_VIEW-KURZTEXT = V_DESC_CODE.
    T_VIEW-EQART    = V_EQART.
    T_VIEW-EARTX    = V_DESC_EQART.
    T_VIEW-TYPBZ    = V_TYPBZ.
    T_VIEW-CONSUMO  = V_CONSUMO.
    T_VIEW-DESVIO   = V_DESVIO.
    APPEND T_VIEW.
    CLEAR: V_TYPBZ, V_CONSUMO, V_DESVIO.
  ENDIF.
*
ENDFORM.
