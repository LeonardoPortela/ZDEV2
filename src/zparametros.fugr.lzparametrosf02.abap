*----------------------------------------------------------------------*
***INCLUDE LZPARAMETROSF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .
  PERFORM MONTAR_ESTRUTURA USING:
       1  ' '   ' '            'TL_SAIDA' 'FILENAME'     'Nome do arquivo'  '80'.
ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.

  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
  WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  IF P_OUTPUTLEN IS INITIAL.
    WA_ESTRUTURA-OUTPUTLEN     = X_CONTADOR.
  ELSE.
    WA_ESTRUTURA-OUTPUTLEN     =  P_OUTPUTLEN.
  ENDIF.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-DDICTXT = 'L'.
  ENDIF.



  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.
  PERFORM F_CARREGAR_EVENTOS USING:
                                 SLIS_EV_USER_COMMAND  'XUSER_COMMAND'.
*                                 SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET'.
**                                 SLIS_EV_TOP_OF_PAGE   'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " f_carregar_eventos
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XPF_STATUS_SET USING EXTAB TYPE KKBLO_T_EXTAB.         "#EC CALLED
  SET PF-STATUS 'STDPOPUP_FULLSCREEN'.
ENDFORM. "XPF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD.

  IF UCOMM EQ '&ONT'.
    LOOP AT TG_SAIDA WHERE MARK EQ 'X'.
      MOVE: TG_SAIDA-FILENAME TO TG_FILENAME-PATHNAME.
      APPEND TG_FILENAME.
      CLEAR: TG_FILENAME.
    ENDLOOP.

  ENDIF.
ENDFORM. "XUSER_COMMAND
