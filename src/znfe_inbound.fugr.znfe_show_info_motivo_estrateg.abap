FUNCTION ZNFE_SHOW_INFO_MOTIVO_ESTRATEG.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CD_APROVACAO) TYPE  ZDE_EST_APROVACAO
*"----------------------------------------------------------------------

  DATA: LC_ID     LIKE  THEAD-TDID,
        LC_NAME   LIKE  THEAD-TDNAME,
        LC_OBJECT LIKE  THEAD-TDOBJECT.

  CK_VIEW_APROVACAO       = ABAP_TRUE.

  SELECT SINGLE * INTO WA_ULTIMA_APROVACAO
    FROM ZIB_NFE_DIST_EAP
   WHERE CD_APROVACAO EQ I_CD_APROVACAO.

  LC_ID     = 'ZNFE'.
  LC_OBJECT = 'ZAPROVACAO'.
  CONCATENATE SY-MANDT I_CD_APROVACAO INTO LC_NAME.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME    = 'ZDM_MOTIVO_REJEICAO_FISCAL'
    TABLES
      VALUES_TAB = IT_DD07V.

  CLEAR: TL_TLINES[].

  READ TABLE IT_DD07V INTO WA_DD07V WITH KEY DOMVALUE_L = WA_ULTIMA_APROVACAO-TP_APROVACAO.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = LC_ID
      LANGUAGE                = SY-LANGU
      NAME                    = LC_NAME
      OBJECT                  = LC_OBJECT
    TABLES
      LINES                   = TL_TLINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  CALL SCREEN 9003 STARTING AT 5 5.

ENDFUNCTION.
