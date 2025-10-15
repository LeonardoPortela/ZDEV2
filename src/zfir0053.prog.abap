*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 12/06/2014                                              &*
*& Descrição: Atualização de dados de fluxo de caixa - previsto       &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         12.06.2014                            &*
*&--------------------------------------------------------------------&*

REPORT  zfir0053.

TABLES: zfit0079.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo.

DATA: BEGIN OF tg_0079 OCCURS 0.
        INCLUDE STRUCTURE zfit0079.
DATA: END OF tg_0079.

DATA: BEGIN OF tg_0119 OCCURS 0.
        INCLUDE STRUCTURE zfit0119.
DATA: END OF tg_0119.

DATA: BEGIN OF tg_lfa1 OCCURS 0,
        lifnr LIKE lfa1-lifnr,
        name1 LIKE lfa1-name1,
      END OF tg_lfa1.

DATA: BEGIN OF tg_kna1 OCCURS 0,
        kunnr LIKE kna1-kunnr,
        name1 LIKE kna1-name1,
      END OF tg_kna1.

DATA: BEGIN OF tg_skat OCCURS 0.
        INCLUDE STRUCTURE skat.
DATA: END OF tg_skat.

DATA: BEGIN OF tg_0109 OCCURS 0.
        INCLUDE STRUCTURE zfit0109.
DATA: END OF tg_0109.

DATA: BEGIN OF tg_0077 OCCURS 0.
        INCLUDE STRUCTURE zfit0077.
DATA: END OF tg_0077.

DATA: BEGIN OF tg_saida OCCURS 0,
        name1      LIKE lfa1-name1,
        txt50      LIKE skat-txt50,
        desc_flx   LIKE zfit0109-descricao,
        opr_numero LIKE zfit0119-opr_numero,
        con_codigo LIKE zfit0119-con_codigo,
        mdo_codigo LIKE zfit0119-mdo_codigo,
        par_tipo   LIKE zfit0119-par_tipo,
        mdo_tipo   LIKE zfit0119-mdo_tipo,
        bukrs_opr  LIKE zfit0119-bukrs_opr,
        agente     LIKE zfit0119-agente,
        regra_val  LIKE zfit0119-regra_val.
        INCLUDE STRUCTURE zfit0079.
DATA END OF tg_saida.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
*      estrutura    TYPE TABLE OF ty_estrutura,
*      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      t_top_aux    TYPE slis_t_listheader,
      tg_sort      TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      tg_fcat      TYPE TABLE OF ty_estrutura,
      wa_sort      TYPE slis_sortinfo_alv,
      vg_contador  TYPE i,
      gs_variant_c TYPE disvariant,
      variante     LIKE disvariant.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs     FOR zfit0079-bukrs OBLIGATORY,
                  s_lifnr     FOR zfit0079-lifnr,
                  s_hkont     FOR zfit0079-hkont,
                  "s_cod       FOR zfit0079-cod_flx,
                  s_budat     FOR zfit0079-budat,
                  s_zfbdt     FOR zfit0079-zfbdt OBLIGATORY,
                  s_prev      FOR zfit0079-tp_prev,
                  s_cls_f     FOR zfit0079-clas_flx,
                  s_cod_f     FOR zfit0079-codigo,
                  s_dt_vrs    FOR zfit0079-dt_base_versao NO INTERVALS NO-EXTENSION OBLIGATORY,
                  s_vrs       FOR zfit0079-versao NO INTERVALS NO-EXTENSION OBLIGATORY.

  PARAMETERS: p_inter TYPE c NO-DISPLAY.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-002.
  PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b5.

INITIALIZATION.
  gs_variant_c-report      = sy-repid.
*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
*
  DATA: vg_repid   LIKE sy-repid,
        vg_variant TYPE disvariant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  variante-report = vg_repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: seleciona_dados,

           organiza_dados,

           chama_alv.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_dados .


  SELECT * FROM zfit0079 INTO TABLE tg_0079
   WHERE bukrs IN s_bukrs
     AND ( ( lifnr IN s_lifnr ) OR ( kunnr IN s_lifnr ) )
     AND budat IN s_budat
     AND zfbdt IN s_zfbdt
     AND codigo IN s_cod_f
     AND tp_prev IN s_prev
     AND clas_flx IN s_cls_f
     AND dt_base_versao IN s_dt_vrs
     AND versao         IN s_vrs
     AND cx_internacional EQ p_inter.


  "XRT
  SELECT * FROM zfit0119 INTO TABLE tg_0119
    WHERE bukrs IN s_bukrs
      AND zfbdt IN s_zfbdt
      AND codigo IN s_cod_f
      AND tp_prev IN s_prev
      AND clas_flx IN s_cls_f
      AND dt_base_versao IN s_dt_vrs
      AND versao         IN s_vrs.

  IF ( tg_0079[] IS INITIAL ) AND ( tg_0119[] IS INITIAL ) .
* msg de erro
    STOP.
  ENDIF.

  IF tg_0079[] IS NOT INITIAL.

    SELECT lifnr name1
      FROM lfa1 INTO TABLE tg_lfa1
      FOR ALL ENTRIES IN tg_0079
      WHERE lifnr = tg_0079-lifnr.

    SELECT kunnr name1
      FROM kna1 INTO TABLE tg_kna1
      FOR ALL ENTRIES IN tg_0079
      WHERE kunnr = tg_0079-kunnr.

    SELECT * FROM skat INTO TABLE tg_skat
      FOR ALL ENTRIES IN tg_0079
      WHERE spras EQ sy-langu AND
            ktopl EQ '0050'   AND
            saknr EQ tg_0079-hkont.

    SELECT * FROM zfit0077 INTO TABLE tg_0077
      FOR ALL ENTRIES IN tg_0079
      WHERE cod_flx EQ tg_0079-cod_flx.

  ENDIF.


ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM organiza_dados .

  SORT: tg_lfa1 BY lifnr,
        tg_kna1 BY kunnr,
        tg_skat BY saknr,
        tg_0077 BY cod_flx.

  REFRESH tg_saida. CLEAR tg_saida.
  LOOP AT tg_0079.

    IF tg_0079-lifnr IS NOT INITIAL.

      READ TABLE tg_lfa1 WITH KEY lifnr = tg_0079-lifnr
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        tg_saida-name1 = tg_lfa1-name1.
      ENDIF.

    ENDIF.

    IF tg_0079-kunnr IS NOT INITIAL.

      READ TABLE tg_kna1 WITH KEY kunnr = tg_0079-kunnr
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        tg_saida-name1 = tg_kna1-name1.
      ENDIF.

    ENDIF.

    CLEAR: tg_0109.
    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
      WHERE codigo = tg_0079-codigo.

    IF sy-subrc = 0.
      tg_saida-desc_flx =  tg_0109-descricao.
    ENDIF.

    READ TABLE tg_skat WITH KEY saknr = tg_0079-hkont
                                BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR: tg_skat.
    ENDIF.

    "READ TABLE tg_0077 WITH KEY cod_flx = tg_0079-cod_flx
    "                            BINARY SEARCH.
    "IF sy-subrc <> 0.
    "  CONTINUE.
    "ENDIF.

    "IF tg_0079-shkzg = 'H'.
    "  tg_0079-dmbtr = tg_0079-dmbtr * ( - 1 ).
    "  tg_0079-dmbe2 = tg_0079-dmbe2 * ( - 1 ).
    "ENDIF.


    MOVE-CORRESPONDING: tg_0079 TO tg_saida,
                        tg_skat TO tg_saida.



    APPEND tg_saida.
    CLEAR tg_saida.
  ENDLOOP.

  "XTR.
  LOOP AT tg_0119.

    CLEAR: tg_saida.

    CLEAR: tg_0109.
    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
      WHERE codigo = tg_0119-codigo.

    IF sy-subrc = 0.
      tg_saida-desc_flx =  tg_0109-descricao.
    ENDIF.

    MOVE-CORRESPONDING: tg_0119 TO tg_saida.

    APPEND tg_saida.
    CLEAR tg_saida.
  ENDLOOP.

ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CHAMA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM chama_alv .

  DATA: wl_layout TYPE slis_layout_alv.



  CHECK NOT tg_saida[] IS INITIAL.

  v_report = sy-repid.
  wl_layout-colwidth_optimize = 'X'.

  PERFORM: ordena_dados_alv,
           monta_campos_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      is_variant              = gs_variant_c
      i_callback_user_command = 'USER_COMMAND'
      it_fieldcat             = tg_fcat[]
      is_layout               = wl_layout
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
      it_sort                 = tg_sort[]
    TABLES
      t_outtab                = tg_saida.

ENDFORM.                    " CHAMA_ALV


FORM user_command  USING r_ucomm      LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN: '&IC1'.

      IF ( rs_selfield-fieldname EQ 'BELNR' ).
        READ TABLE tg_saida INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD tg_saida-belnr.
        SET PARAMETER ID 'BUK' FIELD tg_saida-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.

      IF ( rs_selfield-fieldname EQ 'AUGBL' ).
        READ TABLE tg_saida INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD tg_saida-augbl.
        SET PARAMETER ID 'BUK' FIELD tg_saida-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.
  ENDCASE.

ENDFORM.                    "user_command



*&---------------------------------------------------------------------*
*&      Form  ORDENA_DADOS_ALV
*&---------------------------------------------------------------------*
FORM ordena_dados_alv .

  CLEAR vg_contador.
  PERFORM ordena USING: 'BUKRS'   'X',
                        'LIFNR'   'X',
                        'KUNNR'   'X',
                        'HKONT'   ' ',
                        'CODIGO'  ' ',
                        'BUDAT'   ' ',
                        'ZFBDT'   ' '.




ENDFORM.                    " ORDENA_DADOS_ALV
*&---------------------------------------------------------------------*
*&      Form  ORDENA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0508   text
*      -->P_0509   text
*----------------------------------------------------------------------*
FORM ordena  USING   p_campo
                     p_soma.

  vg_contador = vg_contador + 1.

  CLEAR tg_sort.
  tg_sort-spos      = vg_contador.
  tg_sort-fieldname = p_campo.
  tg_sort-up      = 'X'.
  tg_sort-subtot    = p_soma.
  APPEND tg_sort.

ENDFORM.                    " ORDENA
*&---------------------------------------------------------------------*
*&      Form  MONTA_CAMPOS_ALV
*&---------------------------------------------------------------------*
FORM monta_campos_alv .

  PERFORM alv_preenche_cat USING:
               'BUKRS'          'Empresa'                         '04'  ''     ' '    ' '  ' ', "Status Solic.
               'LIFNR'          'Fornecedor'                      '09'  ''     ' '    ' '  ' ', "Solicitação Nº
               'KUNNR'          'Cliente'                         '10'  ''     ' '    ' '  ' ', "Solicitação Nº
               'NAME1'          'Nome Forn./Cliente'              '35'  ''     ' '    ' '  ' ',
               'CODIGO'         'Cód.Flx.'                        '08'  ''     'X'    ' '  ' ',
               'DESC_FLX'       'Descrição'                       '14'  ''     'X'    ' '  ' ',
               'CLAS_FLX'       'Clas.Flx.'                       '10'  ''     'X'    ' '  ' ',
               'TP_PREV'        'Tp.Prev.'                        '10'  ''     'X'    ' '  ' ',
               'BLART'          'Tp.Docto'                        '04'  ''     ' '    ' '  ' ',
               'BSART'          'Tp.Ped.'                         '10'  ''     'X'    ' '  ' ',
               'AUART'          'Tp.OV.'                          '10'  ''     'X'    ' '  ' ',
               'ZLSPR'          'Bloq.Pgto'                       '10'  ''     'X'    ' '  ' ',
               'ZLSCH'          'Forma.Pgto'                      '10'  ''     'X'    ' '  ' ',
               'HBKID'          'Bco.Empresa'                     '10'  ''     'X'    ' '  ' ',
               'SGTXT'          'Ref.'                            '10'  ''     'X'    ' '  ' ',
               'SGTXT2'         'Ref.2'                           '10'  ''     'X'    ' '  ' ',
               'DEP_RESP'       'Dep.Resp.'                       '10'  ''     'X'    ' '  ' ',
               'ZFBDT'          'Dt. Vcto'                        '10'  ''     ' '    ' '  ' ',
               'HKONT'          'Conta contábil'                  '10'  ''     ' '    ' '  ' ',
               'TXT50'          'Descrição da conta'              '50'  ''     ' '    ' '  ' ',
               'PLANILHA'       'Planilha'                        '10'  ''     ' '    ' '  ' ',
               'PLANILHA_ITM'   'Plan.Itm.'                       '10'  ''     ' '    ' '  ' ',
               'ID_INVOICE'     'Id.Invoice'                      '25'  ''     ' '    ' '  ' ',
               'DS_PORTO'       'Porto'                           '25'  ''     ' '    ' '  ' ',
               'TRADE_ID'       'Chav.Ident.'                     '10'  ''     ' '    ' '  ' ',
               'DOC_IMPOSTO'    'Doc. Imposto'                    '10'  ''     ' '    ' '  ' ',
               'OBJ_KEY_PREV'   'Chv.Lcto Prev.'                  '30'  ''     ' '    ' '  ' ',
               'SEQITEM'        'Item'                            '10'  ''     ' '    ' '  ' ',
               'CD_PREV'        'Cd.Prev.Avulsa'                  '10'  ''     ' '    ' '  ' ',
               'BELNR'          'Doc. Contábil'                   '10'  'X'    ' '    ' '  ' ',
               'BUDAT'          'Dt. Lcto'                        '10'  ''     ' '    ' '  ' ',
               'BLDAT'          'Dt. Dcto'                        '10'  ''     ' '    ' '  ' ',
               'AUGBL'          'Doc. Comp.'                      '10'  'X'    ' '    ' '  ' ',
               'AUGDT'          'Dt. Comp.'                       '10'  ''     ' '    ' '  ' ',
               'WAERS'          'Moeda'                           '5'   ''     ' '    ' '  ' ',
               'DMBTR'          'Valor R$'                        '10'  ''     ' '    'X'  ' ',
               'DMBE2'          'Valor US$'                       '10'  ''     ' '    'X'  ' ',
               'XBLNR'          'Referência'                      '10'  ''     ' '    ' '  ' ',
               'NRO_SOL'        'Nro.Sol.'                        '10'  ''     ' '    ' '  ' ',
               'EBELN'          'Doc.compra'                      '10'  ''     ' '    ' '  ' ',
               'EBELP'          'Item'                            '10'  ''     ' '    ' '  ' ',
               'BSCHL'          'Chv lc'                          '10'  ''     ' '    ' '  ' ',
               'OPR_NUMERO'     'Nro. Operação'                   '13'  ''     ' '    ' '  ' ',
               'CON_CODIGO'     'Contrato'                        '08'  ''     ' '    ' '  ' ',
               'MDO_CODIGO'     'Modalidade'                      '10'  ''     ' '    ' '  ' ',
               'PAR_TIPO'       'Tp.Parcela'                      '10'  ''     ' '    ' '  ' ',
               'MDO_TIPO'       'Tp.Mdo.'                         '07'  ''     ' '    ' '  ' ',
               'BUKRS_OPR'      'Emp.Opr.'                        '10'  ''     ' '    ' '  ' ',
               'AGENTE'         'Agente'                          '15'  ''     ' '    ' '  ' ',
               'REGRA_VAL'      'Regra Vlr.'                      '20'  ''     ' '    ' '  ' ',
               'PROCESSO_ESP'   'Proc. Espec.'                    '10'  ''     'X'    ' '  ' ',
               'SISTEMA_ORIG'   'Sistema Origem'                  '10'  ''     'X'    ' '  ' ',
               'USNAM'          'Usuário'                         '10'  ''     ' '    ' '  ' ',
               'US_PROC'        'Usuario Proc.'                   '10'  ''     'X'    ' '  ' ',
               'DT_ATUAL'       'Dt atual'                        '10'  ''     ' '    ' '  ' ',
               'HR_ATUAL'       'Hr atual'                        '10'  ''     ' '    ' '  ' '.

  PERFORM remove_colunas_null.

*  perform monta_fieldcat using:
*              'STATUS51'         'ZSDT0051' 'Status Solic.' '05'       'STATUS'            , "Status Solic.
*               'NRO_SOL_OV'       'ZSDT0051' text-002        '15'       'NRO_SOL_OV'        . "Solicitação Nº
ENDFORM.                    " MONTA_CAMPOS_ALV
*&---------------------------------------------------------------------*
*&      Form  alv_preenche_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CAMPO    text
*      -->P_DESC     text
*      -->P_TAM      text
*      -->P_HOT      text
*      -->P_ZERO     text
*      -->P_SOMA     text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo  TYPE c
                               p_desc   TYPE c
                               p_tam    TYPE c
                               p_hot    TYPE c
                               p_zero   TYPE c
                               p_soma   TYPE c
                               p_key    TYPE c.


  DATA: wl_fcat TYPE ty_estrutura.

  wl_fcat-tabname   = 'TG_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_l = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-do_sum    = p_soma.
  wl_fcat-key       = p_key.
  IF p_campo = 'ICON'.
    wl_fcat-icon      = 'X'.
  ENDIF.


  APPEND wl_fcat TO tg_fcat.
ENDFORM.                    " ALV_PREENCHE_CAT


FORM remove_colunas_null.

  DATA: wa_fcat TYPE ty_estrutura.

  LOOP AT tg_fcat INTO wa_fcat.

    wa_fcat-no_out = 'X'.

    CASE wa_fcat-fieldname.
        WHEN 'BUKRS'.
        LOOP AT tg_saida WHERE bukrs IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'LIFNR'.
        LOOP AT tg_saida WHERE lifnr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'KUNNR'.
        LOOP AT tg_saida WHERE kunnr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'NAME1'.
        LOOP AT tg_saida WHERE name1 IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'CODIGO'.
        LOOP AT tg_saida WHERE codigo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DESC_FLX'.
        LOOP AT tg_saida WHERE desc_flx IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'CLAS_FLX'.
        LOOP AT tg_saida WHERE clas_flx IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'TP_PREV'.
        LOOP AT tg_saida WHERE tp_prev IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BLART'.
        LOOP AT tg_saida WHERE blart IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BSART'.
        LOOP AT tg_saida WHERE bsart IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'AUART'.
        LOOP AT tg_saida WHERE auart IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'ZLSPR'.
        LOOP AT tg_saida WHERE zlspr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'ZLSCH'.
        LOOP AT tg_saida WHERE zlsch IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'HBKID'.
        LOOP AT tg_saida WHERE hbkid IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'SGTXT'.
        LOOP AT tg_saida WHERE sgtxt IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'SGTXT2'.
        LOOP AT tg_saida WHERE sgtxt2 IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DEP_RESP'.
        LOOP AT tg_saida WHERE dep_resp IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BUDAT'.
        LOOP AT tg_saida WHERE budat IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'ZFBDT'.
        LOOP AT tg_saida WHERE zfbdt IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'HKONT'.
        LOOP AT tg_saida WHERE hkont IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'TXT50'.
        LOOP AT tg_saida WHERE txt50 IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'PLANILHA'.
        LOOP AT tg_saida WHERE planilha IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'PLANILHA_ITM'.
        LOOP AT tg_saida WHERE planilha_itm IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'ID_INVOICE'.
        LOOP AT tg_saida WHERE id_invoice IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DS_PORTO'.
        LOOP AT tg_saida WHERE ds_porto IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'TRADE_ID'.
        LOOP AT tg_saida WHERE trade_id IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DOC_IMPOSTO'.
        LOOP AT tg_saida WHERE doc_imposto IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'SEQITEM'.
        LOOP AT tg_saida WHERE seqitem IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'CD_PREV'.
        LOOP AT tg_saida WHERE cd_prev IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BELNR'.
        LOOP AT tg_saida WHERE belnr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BLDAT'.
        LOOP AT tg_saida WHERE bldat IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'AUGBL'.
        LOOP AT tg_saida WHERE augbl IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'AUGDT'.
        LOOP AT tg_saida WHERE augdt IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'WAERS'.
        LOOP AT tg_saida WHERE waers IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DMBTR'.
        LOOP AT tg_saida WHERE dmbtr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DMBE2'.
        LOOP AT tg_saida WHERE dmbe2 IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'XBLNR'.
        LOOP AT tg_saida WHERE xblnr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'NRO_SOL'.
        LOOP AT tg_saida WHERE nro_sol IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'EBELN'.
        LOOP AT tg_saida WHERE ebeln IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'EBELP'.
        LOOP AT tg_saida WHERE ebelp IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BSCHL'.
        LOOP AT tg_saida WHERE bschl IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'OPR_NUMERO'.
        LOOP AT tg_saida WHERE opr_numero IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'CON_CODIGO'.
        LOOP AT tg_saida WHERE con_codigo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'MDO_CODIGO'.
        LOOP AT tg_saida WHERE mdo_codigo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'PAR_TIPO'.
        LOOP AT tg_saida WHERE par_tipo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'MDO_TIPO'.
        LOOP AT tg_saida WHERE mdo_tipo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BUKRS_OPR'.
        LOOP AT tg_saida WHERE bukrs_opr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'AGENTE'.
        LOOP AT tg_saida WHERE agente IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'REGRA_VAL'.
        LOOP AT tg_saida WHERE regra_val IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'PROCESSO_ESP'.
        LOOP AT tg_saida WHERE processo_esp IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'SISTEMA_ORIG'.
        LOOP AT tg_saida WHERE sistema_orig IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'USNAM'.
        LOOP AT tg_saida WHERE usnam IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'US_PROC'.
        LOOP AT tg_saida WHERE us_proc IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DT_ATUAL'.
        LOOP AT tg_saida WHERE dt_atual IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'HR_ATUAL'.
        LOOP AT tg_saida WHERE hr_atual IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'OBJ_KEY_PREV'.
        LOOP AT tg_saida WHERE obj_key_prev IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
    ENDCASE.

    MODIFY tg_fcat FROM wa_fcat.

  ENDLOOP.


ENDFORM.


*form monta_fieldcat using p_field
**                          p_tab
*                          p_tabref
*                          p_text
*                          p_out
*                          p_ref_field.
***** Se o programa for um ALV, pode aproveitar p/ carregar fieldcat com
** os atributos necessários, caso não se trate de um ALV basta informar o
** campo de referencia, a tabela de referência, o campo  e a tabela.
*
*  clear: s_fieldcat, wa_fcat_lvc.
*  wa_fcat_lvc-fieldname   = s_fieldcat-fieldname   = p_field.
*  wa_fcat_lvc-tabname     = s_fieldcat-tabname     = '<FS_DATA>'.
*  wa_fcat_lvc-ref_table   = s_fieldcat-ref_tabname = p_tabref.
*  wa_fcat_lvc-seltext     = s_fieldcat-seltext_l   = p_text.
*
*  s_fieldcat-seltext_m    = p_text.
*  s_fieldcat-seltext_l    = p_text.
*  s_fieldcat-seltext_s    = p_text.
*
*  wa_fcat_lvc-outputlen   = s_fieldcat-outputlen   = p_out.
*  wa_fcat_lvc-ref_field   = s_fieldcat-ref_fieldname   = p_ref_field.
*
** carrega fieldcat do alv
*  append s_fieldcat.
*
**inclui dados da work-área p/ tabela sem cab.
*  append wa_fcat_lvc to lt_fcat_lvc.
*
*endform.                    " monta_fieldcat
