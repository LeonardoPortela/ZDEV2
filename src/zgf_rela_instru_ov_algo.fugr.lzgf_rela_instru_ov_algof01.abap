*----------------------------------------------------------------------*
***INCLUDE LZGF_RELA_INSTRU_OV_ALGOF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CALL_REPORT_INSTRUCAO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_REPORT_INSTRUCAO_ALV
  USING P_NR_SAFRA  TYPE ZDE_NR_SAFRA
        P_ID_BUKRS  TYPE ZDE_BUKRS_RECEB
        P_ID_BRANCH TYPE ZDE_BRANCH_RECEB.

  CLEAR: IT_LANC[].

  IF P_ID_BRANCH IS INITIAL.

    LC_FILTRO_TEXTO = |Instruções da Safra { P_NR_SAFRA } |.

    TRY.
        EXEC SQL.
          OPEN DOCUMENTOS FOR
              WITH ponto_coleta AS
               (SELECT vbeln, lifnr
                  FROM SAPHANADB.vbpa pc
                 WHERE pc.mandt = :SY-MANDT
                   and pc.parvw = 'PC'),

              cargas AS
               (SELECT ov.nr_ordem_venda, sum(ov.qt_fardos) as qt_fardos
                  FROM SAPHANADB.zsdt0001ov ov, SAPHANADB.zsdt0001cg cg
                 WHERE ov.mandt = :SY-MANDT
                   and ov.mandt = cg.mandt
                   and ov.id_carga = cg.id_carga
                   and cg.tp_status <> 'CA'
                   and cg.tp_produto_carga = '02'
                 group by ov.nr_ordem_venda),

              remessa AS
               (SELECT rm.vgbel, sum(rm.volum) AS qt_fardos
                  FROM SAPHANADB.lips rm
                 WHERE rm.mandt = :SY-MANDT
                 GROUP BY rm.vgbel),

              fatura AS
               (SELECT ft.aubel, sum(ft.volum) as qt_fardos
                  FROM SAPHANADB.vbrp ft
                 WHERE ft.mandt = :SY-MANDT
                   and ft.shkzg = ' '
                   and not exists (select *
                          from SAPHANADB.vbfa vf
                         where vf.vbelv = ft.vbeln
                           and vf.posnv = ft.posnr
                           and vf.vbtyp_n = 'N'
                           and vf.vbtyp_v = 'M')
                 GROUP BY ft.aubel)

              SELECT BC.INSTRUCAO,
                     BC.WERKS AS LOCAL_NEGOCIO,
                     BC.SAFRA AS SAFRA,
                     VI.LGORT AS DEPOSITO,
                     COALESCE(IT.CHARG_ORI,BC.CHARG) AS BLOCO,
                     VI.VBELN AS ORDEM_VENDA,
                     IT.STATUS AS STATUS,
                     CASE
                       WHEN IT.STATUS = 'A' THEN
                        'Aguardando Sol. de Aprovação'
                       WHEN IT.STATUS = 'L' THEN
                        'Liberado'
                       WHEN IT.STATUS = 'D' THEN
                        'Deletado'
                       WHEN IT.STATUS = 'R' THEN
                        'Reprovado'
                       WHEN IT.STATUS = 'P' THEN
                        'Aguardando Aprovação'
                     END AS DS_STATUS,
                     VK.AUART AS TIPO_ORDEM,
                     VI.MATNR,
                     VI.ARKTX,
                     VI.VOLUM TOTAL_FARDOS,
                     VI.VOLEH UNIDADE,
                     IT.NRO_SOL_OV,
                     oc.lifnr,
                     pc.name1,
                     COALESCE(cg.qt_fardos,0) as qt_fardos_em_carga,
                     COALESCE(VI.VOLUM,0) - COALESCE(cg.qt_fardos,0) as qt_fardos_saldo,
                     COALESCE(rm.qt_fardos,0) as qt_fardos_em_remessa,
                     COALESCE(ft.qt_fardos,0) as qt_fardos_em_fatura
                FROM SAPHANADB.ZSDT0045 BC
               INNER JOIN SAPHANADB.MARA MR
                  ON MR.MANDT = :SY-MANDT
                 AND MR.MATNR = BC.MATNR
                 AND MR.MATKL = '700140'
                LEFT JOIN SAPHANADB.ZSDT0066 IT
                  ON IT.MANDT     = BC.MANDT
                 AND IT.INSTRUCAO = BC.INSTRUCAO
                 AND IT.WERKS     = BC.WERKS
                 AND IT.CHARG     = BC.SAFRA
                 AND IT.PONTO_C   = BC.PONTO_C
                 AND IT.CHARG_ORI = BC.CHARG
                LEFT JOIN SAPHANADB.VBAK VK
                  ON VK.MANDT = IT.MANDT
                 AND VK.VBELN = IT.VBELN
                LEFT JOIN SAPHANADB.VBAP VI
                  ON VI.MANDT = IT.MANDT
                 AND VI.VBELN = IT.VBELN
                LEFT JOIN ponto_coleta oc
                  ON oc.vbeln = it.vbeln
                LEFT JOIN SAPHANADB.lfa1 pc
                  on pc.mandt = it.mandt
                 and pc.lifnr = oc.lifnr
                LEFT JOIN cargas cg
                  ON cg.nr_ordem_venda = it.vbeln
                LEFT JOIN remessa rm
                  ON rm.vgbel = it.vbeln
                LEFT JOIN fatura ft
                  ON ft.aubel = it.vbeln
               WHERE 1 = 1
                 AND BC.MANDT = :SY-MANDT
                 AND BC.SAFRA = :P_NR_SAFRA
        ENDEXEC.
      CATCH CX_SY_NATIVE_SQL_ERROR INTO DATA(EXC_REF).
        MESSAGE EXC_REF->GET_TEXT( ) TYPE 'E' RAISING ERRO_SQL.
    ENDTRY.

  ELSE.

    LC_FILTRO_TEXTO = |Instruções da Safra { P_NR_SAFRA } e Local de Expedição { P_ID_BRANCH } |.

    TRY.
        EXEC SQL.
          OPEN DOCUMENTOS FOR
              WITH ponto_coleta AS
               (SELECT vbeln, lifnr
                  FROM SAPHANADB.vbpa pc
                 WHERE pc.mandt = :SY-MANDT
                   and pc.parvw = 'PC'),

              cargas AS
               (SELECT ov.nr_ordem_venda, sum(ov.qt_fardos) as qt_fardos
                  FROM SAPHANADB.zsdt0001ov ov, SAPHANADB.zsdt0001cg cg
                 WHERE ov.mandt = :SY-MANDT
                   and ov.mandt = cg.mandt
                   and ov.id_carga = cg.id_carga
                   and cg.tp_status <> 'CA'
                   and cg.tp_produto_carga = '02'
                 group by ov.nr_ordem_venda),

              remessa AS
               (SELECT rm.vgbel, sum(rm.volum) AS qt_fardos
                  FROM SAPHANADB.lips rm
                 WHERE rm.mandt = :SY-MANDT
                 GROUP BY rm.vgbel),

              fatura AS
               (SELECT ft.aubel, sum(ft.volum) as qt_fardos
                  FROM SAPHANADB.vbrp ft
                 WHERE ft.mandt = :SY-MANDT
                   and ft.shkzg = ' '
                   and not exists (select *
                          from SAPHANADB.vbfa vf
                         where vf.vbelv = ft.vbeln
                           and vf.posnv = ft.posnr
                           and vf.vbtyp_n = 'N'
                           and vf.vbtyp_v = 'M')
                 GROUP BY ft.aubel)

              SELECT BC.INSTRUCAO,
                     BC.WERKS AS LOCAL_NEGOCIO,
                     BC.SAFRA AS SAFRA,
                     VI.LGORT AS DEPOSITO,
                     COALESCE(IT.CHARG_ORI,BC.CHARG) AS BLOCO,
                     VI.VBELN AS ORDEM_VENDA,
                     IT.STATUS AS STATUS,
                     CASE
                       WHEN IT.STATUS = 'A' THEN
                        'Aguardando Sol. de Aprovação'
                       WHEN IT.STATUS = 'L' THEN
                        'Liberado'
                       WHEN IT.STATUS = 'D' THEN
                        'Deletado'
                       WHEN IT.STATUS = 'R' THEN
                        'Reprovado'
                       WHEN IT.STATUS = 'P' THEN
                        'Aguardando Aprovação'
                     END AS DS_STATUS,
                     VK.AUART AS TIPO_ORDEM,
                     VI.MATNR,
                     VI.ARKTX,
                     VI.VOLUM TOTAL_FARDOS,
                     VI.VOLEH UNIDADE,
                     IT.NRO_SOL_OV,
                     oc.lifnr,
                     pc.name1,
                     COALESCE(cg.qt_fardos,0) as qt_fardos_em_carga,
                     COALESCE(VI.VOLUM,0) - COALESCE(cg.qt_fardos,0) as qt_fardos_saldo,
                     COALESCE(rm.qt_fardos,0) as qt_fardos_em_remessa,
                     COALESCE(ft.qt_fardos,0) as qt_fardos_em_fatura
                FROM SAPHANADB.ZSDT0045 BC
               INNER JOIN SAPHANADB.MARA MR
                  ON MR.MANDT = :SY-MANDT
                 AND MR.MATNR = BC.MATNR
                 AND MR.MATKL = '700140'
                LEFT JOIN SAPHANADB.ZSDT0066 IT
                  ON IT.MANDT     = BC.MANDT
                 AND IT.INSTRUCAO = BC.INSTRUCAO
                 AND IT.WERKS     = BC.WERKS
                 AND IT.CHARG     = BC.SAFRA
                 AND IT.PONTO_C   = BC.PONTO_C
                 AND IT.CHARG_ORI = BC.CHARG
                LEFT JOIN SAPHANADB.VBAK VK
                  ON VK.MANDT = IT.MANDT
                 AND VK.VBELN = IT.VBELN
                LEFT JOIN SAPHANADB.VBAP VI
                  ON VI.MANDT = IT.MANDT
                 AND VI.VBELN = IT.VBELN
                LEFT JOIN ponto_coleta oc
                  ON oc.vbeln = it.vbeln
                LEFT JOIN SAPHANADB.lfa1 pc
                  on pc.mandt = it.mandt
                 and pc.lifnr = oc.lifnr
                LEFT JOIN cargas cg
                  ON cg.nr_ordem_venda = it.vbeln
                LEFT JOIN remessa rm
                  ON rm.vgbel = it.vbeln
                LEFT JOIN fatura ft
                  ON ft.aubel = it.vbeln
               WHERE 1 = 1
                 AND BC.MANDT = :SY-MANDT
                 AND BC.SAFRA = :P_NR_SAFRA
                 AND BC.WERKS = :P_ID_BRANCH

        ENDEXEC.
      CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
        MESSAGE EXC_REF->GET_TEXT( ) TYPE 'E' RAISING ERRO_SQL.
    ENDTRY.

  ENDIF.

  DATA: WA_LANC TYPE ZDE_CONS_INSTRUCAO.

  DO.
    EXEC SQL.
      FETCH NEXT DOCUMENTOS INTO
      :WA_LANC-INSTRUCAO,
      :WA_LANC-LOCAL_NEGOCIO,
      :WA_LANC-SAFRA,
      :WA_LANC-DEPOSITO,
      :WA_LANC-BLOCO,
      :WA_LANC-ORDEM_VENDA,
      :WA_LANC-STATUS,
      :WA_LANC-DS_STATUS,
      :WA_LANC-TIPO_ORDEM,
      :WA_LANC-MATNR,
      :WA_LANC-ARKTX,
      :WA_LANC-TOTAL_FARDOS,
      :WA_LANC-UNIDADE,
      :WA_LANC-NRO_SOL_OV,
      :WA_LANC-LIFNR,
      :WA_LANC-NAME1,
      :WA_LANC-QT_FARDOS_EM_CARGA,
      :WA_LANC-qt_fardos_saldo,
      :WA_LANC-QT_FARDOS_EM_REMESSA,
      :WA_LANC-QT_FARDOS_EM_FATURA
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      APPEND WA_LANC TO IT_LANC.
      CLEAR: WA_LANC.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE DOCUMENTOS
  ENDEXEC.

  "Remover Filiais Sem Acesso
  DATA(IT_FILIAIS) = IT_LANC[].
  SORT IT_FILIAIS BY LOCAL_NEGOCIO.
  DELETE ADJACENT DUPLICATES FROM IT_FILIAIS COMPARING LOCAL_NEGOCIO.

  LOOP AT IT_FILIAIS INTO DATA(WA_FILIAL).

    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
      ID 'WERKS' FIELD  WA_FILIAL-LOCAL_NEGOCIO
      ID 'ACTVT' FIELD '03'.    "Alteração

    IF SY-SUBRC IS NOT INITIAL.
      DELETE IT_LANC WHERE LOCAL_NEGOCIO EQ WA_FILIAL-LOCAL_NEGOCIO.
    ENDIF.

  ENDLOOP.

  CALL SCREEN 0100.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
*       build hierarchy-header-information
*----------------------------------------------------------------------*
*      -->P_L_HIERARCHY_HEADER  strucxture for hierarchy-header
*----------------------------------------------------------------------*
FORM BUILD_HIERARCHY_HEADER CHANGING
                               P_HIERARCHY_HEADER TYPE TREEV_HHDR.

  P_HIERARCHY_HEADER-HEADING   = 'Instrução/Safra/Local de Negócio'.
  P_HIERARCHY_HEADER-TOOLTIP   = 'Fluxo de Documentos da Instrução'.
  P_HIERARCHY_HEADER-WIDTH     = 50.
  P_HIERARCHY_HEADER-WIDTH_PIX = ' '.

ENDFORM.                               " build_hierarchy_header

*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_HIERARCHY .

  DATA: L_INST_KEY  TYPE LVC_NKEY,
        L_LOCAL_KEY TYPE LVC_NKEY,
        L_SAFRA_KEY TYPE LVC_NKEY,
        L_LAST_KEY  TYPE LVC_NKEY.

  DATA: LC_INSTRUCAO     TYPE ZSDED030,
        LC_LOCAL_NEGOCIO TYPE VSTEL,
        LC_SAFRA         TYPE CHARG_D.

  DATA: LC_INSTRUCAO_LAST     TYPE ZSDED030,
        LC_LOCAL_NEGOCIO_LAST TYPE VSTEL,
        LC_SAFRA_LAST         TYPE CHARG_D.

  SORT IT_LANC BY INSTRUCAO SAFRA LOCAL_NEGOCIO .

  CLEAR: L_INST_KEY, L_LOCAL_KEY, L_SAFRA_KEY, L_LAST_KEY, LC_INSTRUCAO, LC_INSTRUCAO_LAST,
         LC_LOCAL_NEGOCIO_LAST, LC_LOCAL_NEGOCIO, LC_SAFRA, LC_SAFRA_LAST.

  LOOP AT IT_LANC INTO DATA(WA_LANC).

    LC_INSTRUCAO     = WA_LANC-INSTRUCAO.
    LC_LOCAL_NEGOCIO = WA_LANC-LOCAL_NEGOCIO.
    LC_SAFRA         = WA_LANC-SAFRA.

    IF LC_INSTRUCAO NE LC_INSTRUCAO_LAST.
      LC_INSTRUCAO_LAST = LC_INSTRUCAO.
      PERFORM ADD_INSTRUCAO USING LC_INSTRUCAO '' CHANGING L_INST_KEY.
      CLEAR: LC_SAFRA_LAST, LC_LOCAL_NEGOCIO_LAST.
    ENDIF.

    IF LC_SAFRA NE LC_SAFRA_LAST.
      LC_SAFRA_LAST = LC_SAFRA.
      PERFORM ADD_SAFRA USING LC_SAFRA L_INST_KEY CHANGING L_SAFRA_KEY.
      CLEAR: LC_LOCAL_NEGOCIO_LAST.
    ENDIF.

    IF LC_LOCAL_NEGOCIO NE LC_LOCAL_NEGOCIO_LAST.
      LC_LOCAL_NEGOCIO_LAST = LC_LOCAL_NEGOCIO.
      PERFORM ADD_LOCAL_NEGOCIO USING LC_LOCAL_NEGOCIO L_SAFRA_KEY CHANGING L_LOCAL_KEY.
    ENDIF.

    PERFORM ADD_COMPLETE_LINE USING WA_LANC L_LOCAL_KEY CHANGING L_LAST_KEY.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100' WITH LC_FILTRO_TEXTO.

  IF G_ALV_TREE IS INITIAL.

    CREATE OBJECT G_ALV_TREE
      EXPORTING
        PARENT                      = CL_GUI_CONTAINER=>SCREEN0
        NODE_SELECTION_MODE         = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
        ITEM_SELECTION              = 'X'
        NO_TOOLBAR                  = ABAP_FALSE
        NO_HTML_HEADER              = ABAP_TRUE
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        ILLEGAL_NODE_SELECTION_MODE = 5
        FAILED                      = 6
        ILLEGAL_COLUMN_NAME         = 7
        OTHERS                      = 8.

    DATA L_HIERARCHY_HEADER TYPE TREEV_HHDR.

    PERFORM BUILD_HIERARCHY_HEADER CHANGING L_HIERARCHY_HEADER.

    PERFORM BUILD_FIELDCATALOG.

    CALL METHOD G_ALV_TREE->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_HIERARCHY_HEADER = L_HIERARCHY_HEADER
      CHANGING
        IT_FIELDCATALOG     = GT_FIELDCATALOG
        IT_OUTTAB           = GT_LANC.

    PERFORM CREATE_HIERARCHY.

    CALL METHOD G_ALV_TREE->UPDATE_CALCULATIONS.

    CALL METHOD G_ALV_TREE->FRONTEND_UPDATE.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  IF G_ALV_TREE IS NOT INITIAL.
    G_ALV_TREE->FREE( ).
  ENDIF.
  CLEAR: G_ALV_TREE.

  CLEAR: IT_LANC[], IT_LANC, GT_LANC, GT_LANC[], LC_FILTRO_TEXTO, GT_FIELDCATALOG, GT_FIELDCATALOG[].

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ADD_INSTRUCAO
*&---------------------------------------------------------------------*
FORM ADD_INSTRUCAO  USING P_LC_INSTRUCAO TYPE C
                          P_RELAT_KEY TYPE LVC_NKEY
                    CHANGING P_NODE_KEY TYPE LVC_NKEY.

  DATA: LS_LANC     TYPE ZDE_CONS_INSTRUCAO,
        I_NODE_TEXT	TYPE LVC_VALUE.

  I_NODE_TEXT = P_LC_INSTRUCAO.

  CALL METHOD G_ALV_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = I_NODE_TEXT
      IS_OUTTAB_LINE   = LS_LANC
    IMPORTING
      E_NEW_NODE_KEY   = P_NODE_KEY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_LOTE
*&---------------------------------------------------------------------*
FORM ADD_SAFRA  USING   P_LC_SAFRA TYPE C
                        P_L_INST_KEY TYPE LVC_NKEY
               CHANGING P_L_LOTE_KEY TYPE LVC_NKEY.

  DATA: LS_LANC     TYPE ZDE_CONS_INSTRUCAO,
        I_NODE_TEXT	TYPE LVC_VALUE.

  I_NODE_TEXT = P_LC_SAFRA.

  CALL METHOD G_ALV_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = P_L_INST_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = I_NODE_TEXT
      IS_OUTTAB_LINE   = LS_LANC
    IMPORTING
      E_NEW_NODE_KEY   = P_L_LOTE_KEY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_LOCAL_NEGOCIO
*&---------------------------------------------------------------------*
FORM ADD_LOCAL_NEGOCIO  USING    P_LC_LOCAL_NEGOCIO TYPE C
                                 P_L_SAFRA_KEY TYPE LVC_NKEY
                        CHANGING P_L_LOCAL_KEY TYPE LVC_NKEY.

  DATA: LS_LANC     TYPE ZDE_CONS_INSTRUCAO,
        I_NODE_TEXT	TYPE LVC_VALUE.

  I_NODE_TEXT = P_LC_LOCAL_NEGOCIO.

  CALL METHOD G_ALV_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = P_L_SAFRA_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = I_NODE_TEXT
      IS_OUTTAB_LINE   = LS_LANC
    IMPORTING
      E_NEW_NODE_KEY   = P_L_LOCAL_KEY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_COMPLETE_LINE
*&---------------------------------------------------------------------*
FORM ADD_COMPLETE_LINE  USING    P_WA_LANC TYPE ZDE_CONS_INSTRUCAO
                                 P_L_LOCAL_KEY TYPE LVC_NKEY
                        CHANGING P_L_LAST_KEY TYPE LVC_NKEY.

  DATA I_NODE_TEXT  TYPE LVC_VALUE.

  I_NODE_TEXT = P_WA_LANC-ORDEM_VENDA.

  CALL METHOD G_ALV_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = P_L_LOCAL_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      IS_OUTTAB_LINE   = P_WA_LANC
      I_NODE_TEXT      = I_NODE_TEXT
    IMPORTING
      E_NEW_NODE_KEY   = P_L_LAST_KEY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .

  DATA: LS_FIELDCATALOG TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_CONS_INSTRUCAO'
    CHANGING
      CT_FIELDCAT      = GT_FIELDCATALOG.

  LOOP AT GT_FIELDCATALOG ASSIGNING FIELD-SYMBOL(<FS>).
    CASE <FS>-FIELDNAME.
      WHEN 'INSTRUCAO'.
        <FS>-OUTPUTLEN = 20.
      WHEN 'LOCAL_NEGOCIO'.
        <FS>-OUTPUTLEN = 08.
      WHEN 'SAFRA'.
        <FS>-OUTPUTLEN = 08.
      WHEN 'DEPOSITO'.
        <FS>-OUTPUTLEN = 08.
      WHEN 'BLOCO'.
        <FS>-OUTPUTLEN = 08.
      WHEN 'ORDEM_VENDA'.
        <FS>-OUTPUTLEN = 10.
      WHEN 'STATUS'.
        <FS>-OUTPUTLEN = 04.
      WHEN 'DS_STATUS'.
        <FS>-OUTPUTLEN = 30.
      WHEN 'TIPO_ORDEM'.
        <FS>-OUTPUTLEN = 08.
      WHEN 'MATNR'.
        <FS>-OUTPUTLEN = 10.
      WHEN 'ARKTX'.
        <FS>-OUTPUTLEN = 35.
      WHEN 'UNIDADE'.
        <FS>-OUTPUTLEN = 08.
      WHEN 'NRO_SOL_OV'.
        <FS>-OUTPUTLEN = 10.
      WHEN 'LIFNR'.
        <FS>-OUTPUTLEN = 10.
      WHEN 'NAME1'.
        <FS>-OUTPUTLEN = 35.
      WHEN 'TOTAL_FARDOS' OR
           'QT_FARDOS_EM_CARGA' OR
           'QT_FARDOS_SALDO' OR
           'QT_FARDOS_EM_REMESSA' OR
           'QT_FARDOS_EM_FATURA'.
        <FS>-DO_SUM = 'X'.
        <FS>-H_FTYPE = 'SUM'.
        <FS>-OUTPUTLEN = 20.
    ENDCASE.
  ENDLOOP.

ENDFORM.
