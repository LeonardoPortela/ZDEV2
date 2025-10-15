*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F20
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_status_icons
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_NFE_ALV  text
*----------------------------------------------------------------------*
FORM SET_STATUS_ICONS CHANGING P_NFE_ALV LIKE WA_NFE_ALV.

* The indicator ACTION_REQU
* is set in FU J_1B_NFE_UPDATE_ACTIVE

  IF P_NFE_ALV-MODEL EQ ZIF_DOC_ELETRONICO=>AT_ST_MODEL_MDFE.

*1  A encerrar
*2  Solicitado Encerramento
*3  Encerrado
*4  Erro Encerramento

* COMPLETED
***********************************************************************
    IF P_NFE_ALV-ACTION_REQU CA 'C' AND ( P_NFE_ALV-ENCERRADO NE ICON_WARNING OR WA_NFE_ALV-ENCERRADO IS INITIAL ).
      P_NFE_ALV-STATUS = ICON_COMPLETE.
* ALERT: NF-e ERROR/INCONSISTENCY - USER ACTION REQUIRED
***********************************************************************
    ELSEIF P_NFE_ALV-ACTION_REQU CA '145' AND ( P_NFE_ALV-ENCERRADO NE ICON_WARNING OR WA_NFE_ALV-ENCERRADO IS INITIAL ).
      P_NFE_ALV-STATUS = ICON_ALERT.
* IN PROCESS - USER ACTION REQUIRED
***********************************************************************
    ELSEIF P_NFE_ALV-ACTION_REQU CA '2367890' AND ( P_NFE_ALV-ENCERRADO NE ICON_WARNING  OR WA_NFE_ALV-ENCERRADO IS INITIAL ).
      P_NFE_ALV-STATUS = ICON_WARNING.
* IN PROCESS - NO USER ACTION REQUIRED
***********************************************************************
    ELSE.
      P_NFE_ALV-STATUS = ICON_ACTIVITY.
    ENDIF.

  ELSE.


* COMPLETED
***********************************************************************
    IF P_NFE_ALV-ACTION_REQU CA 'C'.
      P_NFE_ALV-STATUS = ICON_COMPLETE.
* ALERT: NF-e ERROR/INCONSISTENCY - USER ACTION REQUIRED
***********************************************************************
    ELSEIF P_NFE_ALV-ACTION_REQU CA '145'.
      P_NFE_ALV-STATUS = ICON_ALERT.
* IN PROCESS - USER ACTION REQUIRED
***********************************************************************
    ELSEIF P_NFE_ALV-ACTION_REQU CA '2367890'.
      P_NFE_ALV-STATUS = ICON_WARNING.
* IN PROCESS - NO USER ACTION REQUIRED
***********************************************************************
    ELSE.
      P_NFE_ALV-STATUS = ICON_ACTIVITY.
    ENDIF.

  ENDIF.

ENDFORM.                    " set_status_icons
