*----------------------------------------------------------------------*
*   INCLUDE HBRCVT08             " List output
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       FORM FILL_ALV_TAB
*----------------------------------------------------------------------*
FORM FILL_ALV_TAB.

  DATA: L_BUKRS_TEXT LIKE HRCA_COMPANY-COMP_NAME,
        L_BTRTL_TEXT LIKE T001P-BTEXT,
        L_TRANS_TEXT LIKE Q0410-TDESC,
        W_BTRTL      LIKE COLL_TAB-BTRTL.

  SORT COLL_TAB.

  LOOP AT COLL_TAB.
    W_BTRTL = COLL_TAB-BTRTL.   " to be used at 'at new werks' ...
    AT NEW BUKRS.
      PERFORM COMPANYCODE_TEXT_GET USING COLL_TAB-BUKRS
                                   CHANGING L_BUKRS_TEXT.
      ALV_TAB-BUKRST = L_BUKRS_TEXT.
    ENDAT.
    AT NEW WERKS.
      PERFORM RE001P USING COLL_TAB-WERKS W_BTRTL
                     CHANGING L_BTRTL_TEXT.
      ALV_TAB-BTRTLT = L_BTRTL_TEXT.
    ENDAT.
    AT NEW BTRTL.
      PERFORM RE001P USING COLL_TAB-WERKS COLL_TAB-BTRTL
                     CHANGING L_BTRTL_TEXT.
      ALV_TAB-BTRTLT = L_BTRTL_TEXT.
    ENDAT.
    AT NEW TRANS.
      PERFORM RE7BRTP USING    COLL_TAB-TRANS
                      CHANGING Q0410-TDESC.
      MOVE Q0410-TDESC TO L_TRANS_TEXT.
      ALV_TAB-TRANST = L_TRANS_TEXT.

    ENDAT.
    AT NEW PERSG.
      SELECT SINGLE PTEXT INTO ALV_TAB-persgt FROM t501t WHERE persg EQ coll_tab-persg AND sprsl EQ 'PT'.

    ENDAT.
    AT NEW PERSK.
      SELECT SINGLE PTEXT INTO ALV_TAB-perskt FROM t503t WHERE persk EQ coll_tab-persk AND sprsl EQ 'PT'.

    ENDAT.


*   move rest
    MOVE-CORRESPONDING COLL_TAB TO ALV_TAB.
    clear alv_tab-sign.
    APPEND ALV_TAB.
  ENDLOOP.

ENDFORM.                               " FILL_ALV_TAB

*----------------------------------------------------------------------*
*       FORM CREATE_FIELDCAT
*----------------------------------------------------------------------*
FORM CREATE_FIELDCAT.

  DATA: REPID LIKE SY-REPID,
        FUNC_NAME(28).

  REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_BUFFER_ACTIVE = ''
            I_PROGRAM_NAME         = REPID
            I_INTERNAL_TABNAME     = 'ALV_TAB'
*           I_STRUCTURE_NAME       =
            I_INCLNAME             = 'ZHBRCVTR0TOP'
       CHANGING
            CT_FIELDCAT            = FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MOVE 'REUSE_ALV_FIELDCATALOG_MERGE' TO FUNC_NAME.
    PERFORM APPEND_ERROR
                   USING SPACE
                         'E'
                         '005'
                         SY-SUBRC
                         FUNC_NAME
                         SPACE
                         SPACE.
  ENDIF.

ENDFORM.                               " CREATE_FIELDCAT

*----------------------------------------------------------------------*
*       FORM MODIFY_FIELDCAT
*----------------------------------------------------------------------*
FORM MODIFY_FIELDCAT.

  LOOP AT FIELDCAT.
    CASE FIELDCAT-FIELDNAME.
      WHEN 'PERNR'.
        FIELDCAT-SELTEXT_S = 'Pernr  '(056).
        FIELDCAT-SELTEXT_M = 'Pernr '(057).
        FIELDCAT-SELTEXT_L = 'Pernr      '(058).
        FIELDCAT-OUTPUTLEN = 8.
        FIELDCAT-DDICTXT = 'S'.
        FIELDCAT-NO_SUM = 'X'.
      WHEN 'ENAME'.
        FIELDCAT-SELTEXT_S = 'Nome  '(059).
        FIELDCAT-SELTEXT_M = 'Nome '(060).
        FIELDCAT-SELTEXT_L = 'Nome      '(061).
        FIELDCAT-OUTPUTLEN = 40.
        FIELDCAT-DDICTXT = 'L'.
      WHEN 'PERSG'.
        FIELDCAT-SELTEXT_S = 'Grupo'(062).
        FIELDCAT-SELTEXT_M = 'Grupo'(063).
        FIELDCAT-SELTEXT_L = 'Grupo'(064).
        FIELDCAT-OUTPUTLEN = 5.
        FIELDCAT-DDICTXT = 'L'.
      WHEN 'PERSGT'.
        FIELDCAT-SELTEXT_S = 'Descrição Grupo'(065).
        FIELDCAT-SELTEXT_M = 'Descrição Grupo'(066).
        FIELDCAT-SELTEXT_L = 'Descrição Grupo'(067).
        FIELDCAT-OUTPUTLEN = 30.
        FIELDCAT-DDICTXT = 'L'.
      WHEN 'PERSK'.
        FIELDCAT-SELTEXT_S = 'Sub Grupo'(066).
        FIELDCAT-SELTEXT_M = 'Sub Grupo'(067).
        FIELDCAT-SELTEXT_L = 'Sub Grupo'(068).
        FIELDCAT-OUTPUTLEN = 5.
        FIELDCAT-DDICTXT = 'L'.
      WHEN 'PERSKT'.
        FIELDCAT-SELTEXT_S = 'Desc Sub Grupo'(069).
        FIELDCAT-SELTEXT_M = 'Desc Sub Grupo'(070).
        FIELDCAT-SELTEXT_L = 'Desc Sub Grupo'(071).
        FIELDCAT-OUTPUTLEN = 30.
        FIELDCAT-DDICTXT = 'L'.
      WHEN 'TVIAG'.
        fieldcat-no_sum = 'X'.
      WHEN 'DAYS'.
        fieldcat-no_sum = 'X'.
        FIELDCAT-SELTEXT_S = 'Nro.Dias  '(017).
        FIELDCAT-SELTEXT_M = 'Número de Dias '(018).
        FIELDCAT-SELTEXT_L = 'Número de Dias      '(019).
        CLEAR FIELDCAT-REF_FIELDNAME.
        CLEAR FIELDCAT-REF_TABNAME.
      WHEN 'AUSE'.
        fieldcat-no_sum = 'X'.
        FIELDCAT-SELTEXT_S = 'Nro.Ause  '(050).
        FIELDCAT-SELTEXT_M = 'Número de Ausen '(051).
        FIELDCAT-SELTEXT_L = 'Número de Ausencia   '(052).
        CLEAR FIELDCAT-REF_FIELDNAME.
        CLEAR FIELDCAT-REF_TABNAME.
      WHEN 'FERI'.
        fieldcat-no_sum = 'X'.
        FIELDCAT-SELTEXT_S = 'Nro.Fer  '(053).
        FIELDCAT-SELTEXT_M = 'Número de Féri '(054).
        FIELDCAT-SELTEXT_L = 'Número de Férias    '(055).
        CLEAR FIELDCAT-REF_FIELDNAME.
        CLEAR FIELDCAT-REF_TABNAME.

      WHEN 'NBTRP'.
        FIELDCAT-DO_SUM = 'X'.
        FIELDCAT-SELTEXT_S = 'Tot.Pass. '(020).
        FIELDCAT-SELTEXT_M = 'Total Passagens'(021).
        FIELDCAT-SELTEXT_L = 'Total de Passagens  '(022).
        CLEAR FIELDCAT-REF_FIELDNAME.
        CLEAR FIELDCAT-REF_TABNAME.
      WHEN 'VALTT'.
        FIELDCAT-SELTEXT_S = 'Tarifa    '(026).
        FIELDCAT-SELTEXT_M = 'Valor da Tarifa'(027).
        FIELDCAT-SELTEXT_L = 'Valor da Tarifa     '(028).
        fieldcat-no_sum = 'X'.
      WHEN 'VALTT_TOT'.
        FIELDCAT-DO_SUM = 'X'.
        FIELDCAT-SELTEXT_S = 'Tot.Transp'(023).
        FIELDCAT-SELTEXT_M = 'Total Transp.  '(024).
        FIELDCAT-SELTEXT_L = 'Total por Transporte'(025).
      WHEN 'TOTAL_TICKETS'.
        FIELDCAT-DO_SUM = 'X'.
        FIELDCAT-NO_ZERO = 'X'.
        FIELDCAT-SELTEXT_S = 'Tot.Vales '(032).
        FIELDCAT-SELTEXT_M = 'Total de Vales '(033).
        FIELDCAT-SELTEXT_L = 'Total de Vales      '(034).
      WHEN 'REAL_DISC'.
        FIELDCAT-DO_SUM = 'X'.
        FIELDCAT-NO_ZERO = 'X'.
        FIELDCAT-SELTEXT_S = 'Desconto  '(029).
        FIELDCAT-SELTEXT_M = 'Valor Desconto '(030).
        FIELDCAT-SELTEXT_L = 'Valor do Desconto   '(031).
      when 'SIGN'.
        fieldcat-no_sum = 'X'.
        fieldcat-seltext_s = 'Assinatura'(036).
        fieldcat-seltext_m = 'Assinatura     '(037).
        fieldcat-seltext_l = 'Assinatura          '(038).
    ENDCASE.
    MODIFY FIELDCAT.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM DISPLAY_LIST
*----------------------------------------------------------------------*
FORM DISPLAY_LIST.

  DATA: IS_LAYOUT TYPE SLIS_LAYOUT_ALV,
        FUNC_NAME(22),
        l_i_save(1) type c,
        l_i_callback_program like sy-repid.

  l_i_callback_program = 'ZHCM_HRST_48_PA_VT'.
  l_i_save = 'A'.

  IS_LAYOUT-ZEBRA = 'X'.
  IS_LAYOUT-NUMC_SUM = 'X'.
  IS_LAYOUT-SUBTOTALS_TEXT = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
           I_BUFFER_ACTIVE = ''
           i_callback_program = l_i_callback_program
           i_save = l_i_save
           IS_LAYOUT                = IS_LAYOUT
           IT_FIELDCAT              = FIELDCAT[]
      TABLES
           T_OUTTAB                 = ALV_TAB
      EXCEPTIONS
           PROGRAM_ERROR            = 1
           OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MOVE 'REUSE_ALV_GRID_DISPLAY' TO FUNC_NAME.
    PERFORM APPEND_ERROR
                   USING SPACE
                         'E'
                         '005'
                         SY-SUBRC
                         FUNC_NAME
                         SPACE
                         SPACE.
  ENDIF.

ENDFORM.                               " FORM DISPLAY_LIST
