*&---------------------------------------------------------------------*
*&  Include           ZFIR0092_FORM
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor          |Request   |Data      |Descrição                    &*
*&--------------------------------------------------------------------&*
*& NSEGATIN       |DEVK9A2QBW|21.08.2025|Função Z_LES_VERIFICA_PED_ADM&*
*&                                      |ser chamada qdo. Frete = CIF.&*
*&                                      |Chamado:188635.              &*
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*&      Form  CONFIG_RANGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM config_ranges .

  RANGES: l_xblnr    FOR ekes-xblnr. "Nro NFe  "*-IR 185421-03.07.2024-#144903-JT

  REFRESH: it_bukrs,
           it_werks,
           it_charg,
           it_lifnr,
           it_ebeln,
           it_xblnr.  "*-IR 185421-03.07.2024-#144903-JT

  "----------------------------------------------------
  " Empresa
  "----------------------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_bukrs-low
    IMPORTING
      output = p_bukrs-low.

  IF ( p_bukrs-low  IS NOT INITIAL ).
    it_bukrs-sign   = 'I'.
    it_bukrs-option = 'EQ'.
    it_bukrs-low  = p_bukrs-low.
    it_bukrs-high = p_bukrs-low.
    APPEND it_bukrs.
  ENDIF.

  "----------------------------------------------------
  " Filial
  "----------------------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_werks-low
    IMPORTING
      output = p_werks-low.

  IF ( p_werks-low  IS NOT INITIAL ).
    it_werks-sign   = 'I'.
    it_werks-option = 'EQ'.
    it_werks-low  = p_werks-low.
    it_werks-high = p_werks-low.
    APPEND it_werks.
  ENDIF.

  "----------------------------------------------------
  " Safra Lote
  "----------------------------------------------------
  IF ( p_charg-low  IS NOT INITIAL ) AND
     ( p_charg-high IS NOT INITIAL ).
    it_charg-sign   = 'I'.
    it_charg-option = 'BT'.
    it_charg-low  = p_charg-low.
    it_charg-high = p_charg-high.
    APPEND it_charg.
  ELSEIF ( p_charg-low IS NOT INITIAL ).
    it_charg-sign   = 'I'.
    it_charg-option = 'EQ'.
    it_charg-low  = p_charg-low.
    it_charg-high = p_charg-low.
    APPEND it_charg.
  ENDIF.

  "----------------------------------------------------
  " Fornecedor Pedido
  "----------------------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_lifnr-low
    IMPORTING
      output = p_lifnr-low.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_lifnr-high
    IMPORTING
      output = p_lifnr-high.

  IF ( p_lifnr-low  IS NOT INITIAL ) AND
     ( p_lifnr-high IS NOT INITIAL ).
    it_lifnr-sign   = 'I'.
    it_lifnr-option = 'BT'.
    it_lifnr-low  = p_lifnr-low.
    it_lifnr-high = p_lifnr-high.
    APPEND it_lifnr.
  ELSEIF ( p_lifnr-low  IS NOT INITIAL ).
    it_lifnr-sign   = 'I'.
    it_lifnr-option = 'EQ'.
    it_lifnr-low  = p_lifnr-low.
    it_lifnr-high = p_lifnr-low.
    APPEND it_lifnr.
  ENDIF.

  "----------------------------------------------------
  " Pedido
  "----------------------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_ebeln-low
    IMPORTING
      output = p_ebeln-low.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_ebeln-high
    IMPORTING
      output = p_ebeln-high.

  IF ( p_ebeln-low  IS NOT INITIAL ) AND
     ( p_ebeln-high IS NOT INITIAL ).
    it_ebeln-sign   = 'I'.
    it_ebeln-option = 'BT'.
    it_ebeln-low  = p_ebeln-low.
    it_ebeln-high = p_ebeln-high.
    APPEND it_ebeln.
  ELSEIF ( p_ebeln-low  IS NOT INITIAL ).
    it_ebeln-sign   = 'I'.
    it_ebeln-option = 'EQ'.
    it_ebeln-low  = p_ebeln-low.
    it_ebeln-high = p_ebeln-low.
    APPEND it_ebeln.
  ENDIF.

*-IR 185421-03.07.2024-#144903-JT-inicio
  l_xblnr-low  = p_xblnr-low.
  l_xblnr-high = p_xblnr-high.

  SHIFT:    l_xblnr-low  LEFT DELETING LEADING '0',
            l_xblnr-high LEFT DELETING LEADING '0'.
  CONDENSE: l_xblnr-low,
            l_xblnr-high.

  IF l_xblnr-low IS NOT INITIAL.
    l_xblnr-low  = '*' && l_xblnr-low  && '*'.
  ENDIF.
  IF l_xblnr-high IS NOT INITIAL.
    l_xblnr-high = '*' && l_xblnr-high && '*'.
  ENDIF.

  IF ( l_xblnr-low  IS NOT INITIAL ) AND
     ( l_xblnr-high IS NOT INITIAL ).
    it_xblnr-sign   = 'I'.
    it_xblnr-option = 'BT'.
    it_xblnr-low    = l_xblnr-low.
    it_xblnr-high   = l_xblnr-high.
    APPEND it_xblnr.
  ELSEIF ( l_xblnr-low  IS NOT INITIAL ).
    it_xblnr-sign   = 'I'.
    it_xblnr-option = 'CP'.
    it_xblnr-low    = l_xblnr-low.
    APPEND it_xblnr.
  ENDIF.
*-IR 185421-03.07.2024-#144903-JT-fim

ENDFORM.                    " CONFIG_RANGES
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados.

  DATA: r_bsart                   TYPE RANGE OF ekko-bsart,
        tg_tvarvc_ped_gerar_aviso TYPE TABLE OF tvarvc,
        tg_tvarvc_ped_atrib_aviso TYPE TABLE OF tvarvc.

  REFRESH: r_bsart, tg_tvarvc_ped_gerar_aviso, tg_tvarvc_ped_atrib_aviso,
           it_sel_rows. "*-IR 185421-03.07.2024-#144903-JT

**===================================Inicio USER STORY 137420 / AOENNING.
  IF r_gerar IS NOT INITIAL.
    "Seleção stvarv PED_GERAR_AVISO.
    SELECT * FROM tvarvc INTO TABLE tg_tvarvc_ped_gerar_aviso
      WHERE name = 'PED_GERAR_AVISO'.
    IF sy-subrc EQ 0.
      r_bsart = VALUE #( FOR i IN tg_tvarvc_ped_gerar_aviso ( sign = 'I' option = 'EQ' low = i-low ) ).
    ENDIF.
  ENDIF.

  IF r_atrib IS NOT INITIAL.
    "Seleção stvarv PED_ATRIB_AVISO.
    SELECT * FROM tvarvc INTO TABLE tg_tvarvc_ped_atrib_aviso
      WHERE name = 'PED_ATRIB_AVISO'.
    IF sy-subrc EQ 0.
      r_bsart = VALUE #( FOR l IN tg_tvarvc_ped_gerar_aviso ( sign = 'I' option = 'EQ' low = l-low ) ).
    ENDIF.
  ENDIF.
**===================================Fim USER STORY 137420 / AOENNING.

  "Seleção Pedido
  SELECT *
    INTO TABLE it_ekko
    FROM ekko
   WHERE bukrs IN it_bukrs
     AND bsart IN r_bsart
     AND lifnr IN it_lifnr
     AND ebeln IN it_ebeln.

  CHECK it_ekko[] IS NOT INITIAL.

  "Selecão Itens Pedido
  SELECT *
    INTO TABLE it_ekpo
    FROM ekpo
    FOR ALL ENTRIES IN it_ekko
   WHERE ebeln EQ it_ekko-ebeln
     AND loekz EQ ''
     AND bstae NE ''.

  CHECK it_ekpo[] IS NOT INITIAL.

  "Seleção de Divisões do programa de remessas
  SELECT *
    INTO TABLE it_eket
    FROM eket
    FOR ALL ENTRIES IN it_ekpo
   WHERE charg IN it_charg
     AND ebeln = it_ekpo-ebeln
     AND ebelp = it_ekpo-ebelp.

  "De-Para de Centros
  SELECT *
    INTO TABLE it_zsdt_depara_cen
    FROM zsdt_depara_cen
    FOR ALL ENTRIES IN it_ekpo
   WHERE centrov_1   EQ it_ekpo-werks
     AND centro_real IN it_werks.

  "Textos breves de material
  SELECT *
    INTO TABLE it_makt
    FROM makt
    FOR ALL ENTRIES IN it_ekpo
   WHERE matnr = it_ekpo-matnr
     AND spras = sy-langu.

  SELECT *
    INTO TABLE it_lfa1
    FROM lfa1
    FOR ALL ENTRIES IN it_ekko
   WHERE lifnr = it_ekko-lifnr.


ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_dados .

  IF r_atrib IS INITIAL.

    LOOP AT it_ekpo INTO wa_ekpo.

      CLEAR: wa_saida.

      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_ekpo-ebeln.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE it_eket INTO wa_eket WITH KEY ebeln = wa_ekpo-ebeln
                                               ebelp = wa_ekpo-ebelp.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      IF ( wa_ekko-bsart = 'ZGR' ) AND ( wa_ekpo-ebelp NE '10' ).
        CONTINUE.
      ENDIF.

      READ TABLE it_zsdt_depara_cen INTO wa_zsdt_depara_cen WITH KEY centrov_1 =  wa_ekpo-werks.

      IF ( sy-subrc NE 0 ) OR ( wa_zsdt_depara_cen-centro_real NOT IN it_werks ).
        CONTINUE.
      ENDIF.

      wa_saida-bsart  = wa_ekko-bsart.
      wa_saida-ebeln  = wa_ekko-ebeln.
      wa_saida-lifnr  = wa_ekko-lifnr.

      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_saida-lifnr.
      IF sy-subrc = 0.
        wa_saida-name1_forn = wa_lfa1-name1.
      ENDIF.

      wa_saida-matnr  = wa_ekpo-matnr.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_saida-matnr.
      IF sy-subrc = 0.
        wa_saida-maktx = wa_makt-maktx.
      ENDIF.

      wa_saida-ebelp  = wa_ekpo-ebelp.
      wa_saida-werks  = wa_ekpo-werks.
      wa_saida-branch = wa_zsdt_depara_cen-centro_real.

      wa_saida-lgort  = wa_ekpo-lgort.
      wa_saida-charg  = wa_eket-charg.

      APPEND wa_saida TO it_saida.

    ENDLOOP.

  ELSE.

    "Confirmações pedido
    "------------------------------------------------------------------------------"
    "Busca Aviso dados do aviso existente
    "------------------------------------------------------------------------------"
    PERFORM zf_buscar_rel_pedido_nota.

    LOOP AT it_buscar_aviso INTO DATA(wa_buscar_aviso).

      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_buscar_aviso-ebeln
                                               ebelp = wa_buscar_aviso-ebelp.

      CLEAR: wa_saida.

      wa_saida-xblnr = wa_buscar_aviso-xblnr.
      wa_saida-vbeln = wa_buscar_aviso-vbeln. "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>

      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_ekpo-ebeln.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE it_eket INTO wa_eket WITH KEY ebeln = wa_ekpo-ebeln
                                               ebelp = wa_ekpo-ebelp.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      IF ( wa_ekko-bsart = 'ZGR' ) AND ( wa_ekpo-ebelp NE '10' ).
        CONTINUE.
      ENDIF.

      READ TABLE it_zsdt_depara_cen INTO wa_zsdt_depara_cen WITH KEY centrov_1 =  wa_ekpo-werks.

      IF ( sy-subrc NE 0 ) OR ( wa_zsdt_depara_cen-centro_real NOT IN it_werks ).
        CONTINUE.
      ENDIF.

      wa_saida-bsart  = wa_ekko-bsart.
      wa_saida-ebeln  = wa_ekko-ebeln.
      wa_saida-lifnr  = wa_ekko-lifnr.

      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_saida-lifnr.
      IF sy-subrc = 0.
        wa_saida-name1_forn = wa_lfa1-name1.
      ENDIF.

      wa_saida-matnr  = wa_ekpo-matnr.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_saida-matnr.
      IF sy-subrc = 0.
        wa_saida-maktx = wa_makt-maktx.
      ENDIF.

      wa_saida-ebelp  = wa_ekpo-ebelp.
      wa_saida-werks  = wa_ekpo-werks.
      wa_saida-branch = wa_zsdt_depara_cen-centro_real.

      wa_saida-lgort  = wa_ekpo-lgort.
      wa_saida-charg  = wa_eket-charg.

      APPEND wa_saida TO it_saida.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_dados .

*-CS2022001149-29.03.2023-#103469-JT-inicio
  vg_eindt_ini = sy-datum - 2.
  vg_eindt_fim = sy-datum.
*-CS2022001149-29.03.2023-#103469-JT-fim

  CALL SCREEN 0101.
ENDFORM.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_dados .
  REFRESH:  it_saida,
            it_saida_aviso,
            it_ekko,
            it_ekpo,
            it_ekbe,
            it_eket,
            it_zsdt_depara_cen.
ENDFORM.                    " LIMPAR_DADOS

FORM limpar_dados_aviso .

  REFRESH: it_dados_transp, "Dados Transp
           it_dados_nf,     "Dados NF.
           it_dados_aviso,  "Dados Aviso Rec.
           it_dados_veic.   "Dados Modal

  CLEAR: wa_dados_aviso,  "Dados Aviso Rec.
         wa_dados_veic,   "Dados Modal
         wa_dados_transp,
         wa_dados_nf.

ENDFORM.                    " LIMPAR_DADOS


*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM criar_alv_0101 .

  FREE: wa_fcat, it_fcat.

  IF r_atrib IS INITIAL.

    PERFORM estrutura_alv USING:

        0  'EKKO'  'BSART' 'IT_SAIDA' 'BSART'       'Tp. Pedido'       '10' ' '    '' ' ' ' ' ' ',
        1  'EKKO'  'EBELN' 'IT_SAIDA' 'EBELN'       'Pedido'           '10' ' '    '' ' ' ' ' 'X',
        2  'EKKO'  'LIFNR' 'IT_SAIDA' 'LIFNR'       'Fornecedor'       '10' ' '    '' ' ' ' ' ' ',
        2  'LFA1'  'NAME1' 'IT_SAIDA' 'NAME1_FORN'  'Nome Fornecedor'  '35' ' '    '' ' ' ' ' ' ',
        3  'EKPO'  'MATNR' 'IT_SAIDA' 'MATNR'       'Material'         '10' ' '    '' ' ' ' ' ' ',
        3  'MAKT'  'MAKTX' 'IT_SAIDA' 'MAKTX'       'Desc.Mat.'        '30' ' '    '' ' ' ' ' ' ',
        4  'EKPO'  'EBELP' 'IT_SAIDA' 'EBELP'       'Item'             '10' ' '    '' ' ' 'C' ' ',
        5  'EKPO'  'WERKS' 'IT_SAIDA' 'WERKS'       'Centro'           '10' ' '    '' ' ' ' ' ' ',
        6  'EKPO'  'LGORT' 'IT_SAIDA' 'LGORT'       'Deposito'         '10' ' '    '' ' ' ' ' ' ',
        7  'EKET'  'CHARG' 'IT_SAIDA' 'CHARG'       'Lote'             '12' ' '    '' ' ' ' ' ' '.

  ELSE.

    PERFORM estrutura_alv USING:
        0  'EKKO'  'BSART' 'IT_SAIDA' 'BSART'       'Tp. Pedido'       '10' ' '    '' ' ' ' ' ' ',
        1  'EKKO'  'EBELN' 'IT_SAIDA' 'EBELN'       'Pedido'           '10' ' '    '' ' ' ' ' 'X',
        1  'EKES'  'XBLNR' 'IT_SAIDA' 'XBLNR'       'Nota Ref.'        '10' ' '    '' ' ' ' ' ' ',
        1  'EKES'  'VBELN' 'IT_SAIDA' 'VBELN'       'Aviso'            '10' ' '    '' ' ' ' ' ' ',
        2  'EKKO'  'LIFNR' 'IT_SAIDA' 'LIFNR'       'Fornecedor'       '10' ' '    '' ' ' ' ' ' ',
        2  'LFA1'  'NAME1' 'IT_SAIDA' 'NAME1_FORN'  'Nome Fornecedor'  '35' ' '    '' ' ' ' ' ' ',
        3  'EKPO'  'MATNR' 'IT_SAIDA' 'MATNR'       'Material'         '10' ' '    '' ' ' ' ' ' ',
        3  'MAKT'  'MAKTX' 'IT_SAIDA' 'MAKTX'       'Desc.Mat.'        '30' ' '    '' ' ' ' ' ' ',
        4  'EKPO'  'EBELP' 'IT_SAIDA' 'EBELP'       'Item'             '10' ' '    '' ' ' 'C' ' ',
        5  'EKPO'  'WERKS' 'IT_SAIDA' 'WERKS'       'Centro'           '10' ' '    '' ' ' ' ' ' ',
        6  'EKPO'  'LGORT' 'IT_SAIDA' 'LGORT'       'Deposito'         '10' ' '    '' ' ' ' ' ' ',
        7  'EKET'  'CHARG' 'IT_SAIDA' 'CHARG'       'Lote'             '12' ' '    '' ' ' ' ' ' '.

  ENDIF.

ENDFORM.                    " CRIAR_ALV_0101

FORM estrutura_alv USING VALUE(p_col_pos)       TYPE i
                         VALUE(p_ref_tabname)   LIKE dd02d-tabname
                         VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                         VALUE(p_tabname)       LIKE dd02d-tabname
                         VALUE(p_field)         LIKE dd03d-fieldname
                         VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                         VALUE(p_outputlen)
                         VALUE(p_edit)
                         VALUE(p_sum)
                         VALUE(p_emphasize)
                         VALUE(p_just)
                         VALUE(p_hotspot).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM estrutura_alv_log USING VALUE(p_col_pos)       TYPE i
                             VALUE(p_ref_tabname)   LIKE dd02d-tabname
                             VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                             VALUE(p_tabname)       LIKE dd02d-tabname
                             VALUE(p_field)         LIKE dd03d-fieldname
                             VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                             VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura

*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_AVISO_REC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM criar_alv_aviso_rec .
  FREE: wa_fcat, it_fcat.


  PERFORM estrutura_alv USING:
      1   ''  ' ' 'IT_SAIDA_AVISO' 'ICON'         'Log'                 '03' ' '    '' ' ' ' ' 'X',
      2   ''  ' ' 'IT_SAIDA_AVISO' 'PLACA_CAV'    'Placa'               '09' ' '    '' ' ' ' ' ' ',
      3   ''  ' ' 'IT_SAIDA_AVISO' 'VERUR    '    'Nº NF-e'             '15' ' '    '' ' ' ' ' ' ',
      4   ''  ' ' 'IT_SAIDA_AVISO' 'VBELN'        'Aviso Recebimento'   '15' ' '    '' ' ' ' ' 'X',
      5   ''  ' ' 'IT_SAIDA_AVISO' 'BTGEW'        'Quantidade'          '10' ' '    '' ' ' ' ' ' ',
      6   ''  ' ' 'IT_SAIDA_AVISO' 'ERDAT'        'Dt. Movimento'       '13' ' '    '' ' ' ' ' ' ',
      7   ''  ' ' 'IT_SAIDA_AVISO' 'INCO1'        'Tp. Frete'           '09' ' '    '' ' ' ' ' ' ',
      8   ''  ' ' 'IT_SAIDA_AVISO' 'ROUTE'        'Itinerario'          '10' ' '    '' ' ' '' ' ',
      9   ''  ' ' 'IT_SAIDA_AVISO' 'AGENTE_FRETE' 'Agente Frete'        '12' ' '    '' ' ' ' ' ' ',
      10   ''  ' ' 'IT_SAIDA_AVISO' 'KBETR'        'Vlr. Frete'          '10' ' '    '' ' ' ' ' ' ',
      11  ''  ' ' 'IT_SAIDA_AVISO' 'TRANSP'       'Doc. Transp.'        '12' ' '    '' ' ' 'C' 'X',
      12  ''  ' ' 'IT_SAIDA_AVISO' 'DOCCUS'       'Doc. Custo'          '12' ' '    '' ' ' 'C' 'X',
      13  ''  ' ' 'IT_SAIDA_AVISO' 'OVSERV'       'Ov. Serv.'           '12' ' '    '' ' ' 'C' 'X',
      14  ''  ' ' 'IT_SAIDA_AVISO' 'FATSERV'      'Fatura. Serv.'       '12' ' '    '' ' ' 'C' 'X',
      15  ''  ' ' 'IT_SAIDA_AVISO' 'DACTE'        'Dacte.'              '12' ' '    '' ' ' 'C' 'X',
      16  ''  ' ' 'IT_SAIDA_AVISO' 'DAMDFE'       'Damdfe.'             '12' ' '    '' ' ' 'C' 'X'.


ENDFORM.                    " CRIAR_ALV_AVISO_REC


FORM criar_alv_dados_aviso.
  FREE: wa_fcat, it_fcat.

  IF r_atrib IS NOT INITIAL.

    PERFORM estrutura_alv USING:

      0  'ZLEST0109'  'EBELP'      'IT_DADOS_AVISO' 'EBELP'       'Item Pedido'        '10' ' '    '' ' ' ' ' ' ',
      1  'ZLEST0109'  'MATNR'      'IT_DADOS_AVISO' 'MATNR'       'Material'           '20' ' '    '' ' ' ' ' ' ',
      0  'EKES'       'VBELN'      'IT_DADOS_AVISO' 'VBELN'       'Nro. Aviso'         '10' ' '    '' ' ' ' ' ' ',
      2  'ZLEST0109'  'QTDE_AVISO' 'IT_DADOS_AVISO' 'QTDE_AVISO'  'Qtde. Aviso'        '10' ' '    '' ' ' ' ' ' ',
      3  'ZLEST0109'  'UNIDADE'    'IT_DADOS_AVISO' 'UNIDADE'     'UN.'                '7'  ' '    '' ' ' ' ' ' '.

  ELSE.

    PERFORM estrutura_alv USING:

        0  'ZLEST0109'  'EBELP'      'IT_DADOS_AVISO' 'EBELP'       'Item Pedido'        '10' ' '    '' ' ' ' ' ' ',
        1  'ZLEST0109'  'MATNR'      'IT_DADOS_AVISO' 'MATNR'       'Material'           '20' ' '    '' ' ' ' ' ' ',
        2  'ZLEST0109'  'QTDE_AVISO' 'IT_DADOS_AVISO' 'QTDE_AVISO'  'Qtde. Aviso'        '10' ' '    '' ' ' ' ' ' ',
        3  'ZLEST0109'  'UNIDADE'    'IT_DADOS_AVISO' 'UNIDADE'     'UN.'                '7'  ' '    '' ' ' ' ' ' '.

  ENDIF.

ENDFORM.                    " CRIAR_ALV_AVISO_REC


FORM criar_alv_dados_veic.

  FREE: wa_fcat, it_fcat.
  SELECT SINGLE *
     FROM lfa1
     INTO @DATA(wlfa1)
     WHERE lifnr = @wa_dados_transp-agente_frete.

  IF wlfa1-ktokk = 'ZFIC'.
    vg_tipo_frete = 'CIF'.
  ELSE.
    vg_tipo_frete = 'CPT'.
  ENDIF.
  IF vg_tipo_frete NE 'CPT'.
    PERFORM estrutura_alv USING:
        0  'ZLEST0002'  'PC_VEICULO'     'IT_DADOS_VEIC'  'PC_VEICULO'        'Placa'       '10'  'X'    '' ' ' ' ' ' ',
        1  'ZLEST0002'  'CD_CIDADE'      'IT_DADOS_VEIC'  'CD_CIDADE'         'Cidade'      '12'  ' '    '' ' ' ' ' ' ',
        2  ''           'CD_UF'          'IT_DADOS_AVISO' 'CD_UF'             'UF'          '03'  ' '    '' ' ' ' ' ' ',
        3  'ZLEST0002'  'CD_RENAVAM'     'IT_DADOS_AVISO' 'CD_RENAVAM'        'Renavam'     '10'  ' '    '' ' ' ' ' ' ',
        3  'ZLEST0002'  'PROPRIETARIO'   'IT_DADOS_AVISO' 'PROPRIETARIO'      'Cod. Prop.'  '12'  ' '    '' ' ' ' ' ' ',
        3  'LFA1'       'NAME1'          'IT_DADOS_AVISO' 'DES_PROPRIETARIO'  'Nome Prop.'  '15'  ' '    '' ' ' ' ' ' ',
        3  ''           ''               'IT_DADOS_AVISO' 'TP_VEICULO'        'Tp. Veic.'   '08'  ' '    '' ' ' ' ' ' ',
        3  ''           ''               'IT_DADOS_AVISO' 'CNPJ_CPF_PROP'     'CNPJ/CPF'    '16'  ' '    '' ' ' ' ' ' '.
  ELSE.
    PERFORM estrutura_alv USING:
        0  'ZLEST0002'  'PC_VEICULO'     'IT_DADOS_VEIC'  'PC_VEICULO'        'Placa'       '10'  'X'    '' ' ' ' ' ' ',
        1  'ZLEST0002'  'CD_CIDADE'      'IT_DADOS_VEIC'  'CD_CIDADE'         'Cidade'      '12'  ' '    '' ' ' ' ' ' ',
        2  ''           'REGION'        'IT_DADOS_AVISO' 'CD_UF'              'UF'          '03'  ' '    '' ' ' ' ' ' ',
        3  'ZLEST0002'  'CD_RENAVAM'     'IT_DADOS_AVISO' 'CD_RENAVAM'        'Renavam'     '10'  ' '    '' ' ' ' ' ' ',
        3  'ZLEST0002'  'PROPRIETARIO'   'IT_DADOS_AVISO' 'PROPRIETARIO'      'Cod. Prop.'  '12'  ' '    '' ' ' ' ' ' ',
        3  'LFA1'       'NAME1'          'IT_DADOS_AVISO' 'DES_PROPRIETARIO'  'Nome Prop.'  '15'  ' '    '' ' ' ' ' ' ',
        3  ''           ''               'IT_DADOS_AVISO' 'TP_VEICULO'        'Tp. Veic.'   '08'  ' '    '' ' ' ' ' ' ',
        3  ''           ''               'IT_DADOS_AVISO' 'CNPJ_CPF_PROP'     'CNPJ/CPF'    '16'  ' '    '' ' ' ' ' ' '.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_AVISO_REC

*&---------------------------------------------------------------------*
*&      Form  GERAR_AVISO_REC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM abrir_aviso_rec.

  info_aviso_tab-activetab = av_tb01.
  aviso_dynnr_000 = aviso_rec_0103.

  CALL SCREEN 0102 STARTING AT 10 02 ENDING AT 120 25.
ENDFORM.                    " GERAR_AVISO_REC

*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos_0101 OUTPUT.

  IF wa_cont_ped IS INITIAL.

    CLEAR: wa_layout, wa_variante.

    PERFORM criar_alv_0101.

    wa_layout-zebra      = abap_true.
    wa_layout-stylefname = 'FIELD_STYLE'.
    wa_variante-report  = sy-repid.

    CREATE OBJECT wa_cont_ped
      EXPORTING
        container_name = 'CC_PEDIDOS'.

    CREATE OBJECT wa_alv_ped
      EXPORTING
        i_parent = wa_cont_ped.

    "Create toolbar for grid.
    CREATE OBJECT obj_toolbar
      EXPORTING
        io_alv_grid = wa_alv_ped.

    "CREATE OBJECT GR_EVENT_HANDLER.

    "Register event handler
    SET HANDLER: obj_toolbar->on_toolbar                FOR wa_alv_ped,
                 obj_toolbar->handle_user_command       FOR wa_alv_ped.
    "GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR WA_ALV.

    "Excluir Buttons Toolbar
    REFRESH: it_exclude_fcode.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD wa_alv_ped->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = wa_variante
        i_save               = 'X'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_outtab            = it_saida
        it_fieldcatalog      = it_fcat.

    CALL METHOD wa_alv_ped->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD wa_alv_ped->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.



    SET HANDLER: lcl_event_handler_ped=>catch_hotspot  FOR wa_alv_ped,
                 lcl_event_handler_ped=>user_command   FOR wa_alv_ped,
                 lcl_event_handler_ped=>double_click  FOR wa_alv_ped,
                 lcl_event_handler_ped=>on_data_changed  FOR wa_alv_ped.


  ELSE.

    REFRESH: it_fcat[].
    CALL METHOD wa_alv_ped->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = it_fcat[].

    READ TABLE it_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
                                          WITH KEY fieldname = 'XBLNR'.

    IF sy-subrc IS NOT INITIAL AND r_gerar IS INITIAL.

      REFRESH: it_fcat.
      PERFORM criar_alv_0101.

    ELSEIF sy-subrc IS INITIAL AND r_gerar IS NOT INITIAL.

      REFRESH: it_fcat.
      PERFORM criar_alv_0101.

    ENDIF.

    CALL METHOD wa_alv_ped->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fcat[].

    CALL METHOD wa_alv_ped->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

*-IR 185421-03.07.2024-#144903-JT-inicio
  PERFORM f_marcar_linhas.
*-IR 185421-03.07.2024-#144903-JT-fim

  IF wa_cont_aviso_rec IS INITIAL.

    CLEAR: wa_layout, wa_variante.

    PERFORM criar_alv_aviso_rec.

    wa_layout-zebra      = abap_true.
    wa_layout-stylefname = 'FIELD_STYLE'.
    wa_variante-report  = sy-repid.

    CREATE OBJECT wa_cont_aviso_rec
      EXPORTING
        container_name = 'CC_AVISO_REC'.

    CREATE OBJECT wa_alv_aviso_rec
      EXPORTING
        i_parent = wa_cont_aviso_rec.

    "Create toolbar for grid.
    CREATE OBJECT obj_toolbar_aviso
      EXPORTING
        io_alv_grid = wa_alv_aviso_rec.


    "Register event handler
    SET HANDLER: obj_toolbar_aviso->on_toolbar                FOR wa_alv_aviso_rec,
                 obj_toolbar_aviso->handle_user_command       FOR wa_alv_aviso_rec.
    "GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR WA_ALV.

    "Excluir Buttons Toolbar
    REFRESH: it_exclude_fcode.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD wa_alv_aviso_rec->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = wa_variante
        i_save               = 'X'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_outtab            = it_saida_aviso
        it_fieldcatalog      = it_fcat.

    CALL METHOD wa_alv_aviso_rec->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD wa_alv_aviso_rec->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0101=>catch_hotspot            FOR wa_alv_aviso_rec,
                 lcl_event_handler_0101=>on_data_changed_finished FOR wa_alv_aviso_rec,
                 lcl_event_handler_0101=>on_f4                    FOR wa_alv_aviso_rec,
                 lcl_event_handler_0101=>on_data_changed          FOR wa_alv_aviso_rec.


  ELSE.
    CALL METHOD wa_alv_aviso_rec->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.                 " CRIAR_OBJETOS  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos_0103 OUTPUT.

  IF wa_cont_dados_aviso IS INITIAL.

    CLEAR: wa_layout, wa_variante.

    PERFORM criar_alv_dados_aviso.

    wa_layout-zebra      = abap_true.
    wa_layout-stylefname = 'FIELD_STYLE'.
    wa_variante-report  = sy-repid.

    CREATE OBJECT wa_cont_dados_aviso
      EXPORTING
        container_name = 'CC_DADOS_AVISO'.

    CREATE OBJECT wa_alv_dados_aviso
      EXPORTING
        i_parent = wa_cont_dados_aviso.

    "Create toolbar for grid.
    CREATE OBJECT obj_toolbar_dados_aviso
      EXPORTING
        io_alv_grid = wa_alv_dados_aviso.


    "Register event handler
    SET HANDLER: obj_toolbar_dados_aviso->on_toolbar                FOR wa_alv_dados_aviso,
                 obj_toolbar_dados_aviso->handle_user_command       FOR wa_alv_dados_aviso.
    "GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR WA_ALV.

    "Excluir Buttons Toolbar
    REFRESH: it_exclude_fcode.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD wa_alv_dados_aviso->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = wa_variante
        i_save               = 'X'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_outtab            = it_dados_aviso
        it_fieldcatalog      = it_fcat.

    CALL METHOD wa_alv_dados_aviso->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.


  ELSE.
    CALL METHOD wa_alv_dados_aviso->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.                 " CRIAR_OBJETOS_0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_ITENS_AVISO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM preenche_itens_aviso USING p_wa_saida TYPE ty_saida.




ENDFORM.                    " PREENCHE_ITENS_AVISO

*&---------------------------------------------------------------------*
*&      Form  CARREGA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_dados.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD wa_alv_ped->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  IF lines( it_sel_rows ) NE 1.
    MESSAGE 'Selecione apenas um item!' TYPE  'S'.
    EXIT.
  ENDIF.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    CLEAR: wa_saida.
    READ TABLE it_saida INTO wa_saida INDEX wa_sel_rows-index.

    PERFORM preenche_dados USING wa_saida.
  ENDLOOP.

  PERFORM abrir_aviso_rec.

ENDFORM.                    " CARREGA_DADOS

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_CHAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM completa_chave .

  DATA: zcont_caract        TYPE p,
        var_cnpj            TYPE j_1bbranch-stcd1,
        wa_info_c           TYPE kna1,
        wa_info_part        TYPE lfa1,
        t_kna1              TYPE TABLE OF kna1 WITH HEADER LINE,
        t_lfa1              TYPE TABLE OF lfa1 WITH HEADER LINE,
        p_stcd1             TYPE j_1bstcd1,
        p_lifnr_dest        TYPE lfa1-lifnr,
        vg_active           TYPE j_1bnfe_active,
        wa_zib_nfe_dist_ter TYPE zib_nfe_dist_ter,
        wa_zib_nfe_dist_itm TYPE zib_nfe_dist_itm.


  CLEAR: zcont_caract.
*====================================inicio user story 72971 / 07.03.2022 - anderson oenning

  IF ( wa_dados_nf-chave IS NOT INITIAL ) AND ( wa_dados_nf-cliente IS INITIAL ).

    wa_dados_nf-stcd1   = wa_dados_nf-chave+06(14).
    wa_dados_nf-modelo  = wa_dados_nf-chave+20(2).
    wa_dados_nf-serie   = wa_dados_nf-chave+22(3).
    wa_dados_nf-numero  = wa_dados_nf-chave+25(9).
    wa_dados_nf-docnum9 = wa_dados_nf-chave+34(9).
    wa_dados_nf-cdv     = wa_dados_nf-chave+43(1).

    p_stcd1 = wa_dados_nf-chave+06(14).

    IF wa_dados_nf-partyp NE 'V'.

      CALL FUNCTION 'Z_PARCEIRO_CNPJ'
        EXPORTING
          p_stcd1        = p_stcd1
          p_verifica_cad = 'X'
        TABLES
          t_kna1         = t_kna1.

      IF t_kna1[] IS NOT INITIAL.
        READ TABLE t_kna1 INDEX 1.

      ELSE.

        DATA: p_stcd2 TYPE  j_1bcpf.
        p_stcd2 = p_stcd1.

        CALL FUNCTION 'Z_PARCEIRO_CPF'
          EXPORTING
            p_stcd2        = p_stcd2
            p_verifica_cad = 'X'
          TABLES
            t_kna1         = t_kna1.

        IF t_kna1[] IS NOT INITIAL.
          READ TABLE t_kna1 INDEX 1.
          wa_dados_nf-cliente = t_kna1-kunnr.
        ENDIF.

      ENDIF.

    ELSE.

      CALL FUNCTION 'Z_PARCEIRO_CNPJ'
        EXPORTING
          p_stcd1        = p_stcd1
          p_verifica_cad = 'X'
          p_partyp       = 'X'
        TABLES
          t_lfa1         = t_lfa1.

      IF t_lfa1[] IS NOT INITIAL.
        READ TABLE t_lfa1 INDEX 1.
        "WA_DADOS_NF-CLIENTE = T_LFA1-LIFNR.
        wa_dados_nf-cliente = t_lfa1-lifnr.

      ELSE.

        p_stcd2 = p_stcd1.

        CALL FUNCTION 'Z_PARCEIRO_CPF'
          EXPORTING
            p_stcd2        = p_stcd2
            p_verifica_cad = 'X'
            p_partyp       = 'X'
          TABLES
            t_lfa1         = t_lfa1.

        IF t_lfa1[] IS NOT INITIAL.
          READ TABLE t_lfa1 INDEX 1.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  IF wa_dados_nf-cliente IS NOT INITIAL.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_dados_nf-cliente
        p_partype    = wa_dados_nf-partyp
      CHANGING
        wa_info_part = wa_info_part
        wa_info_c    = wa_info_c.

    IF wa_dados_nf-partyp EQ 'V'.
      cte_text_cliente_nome = wa_info_part-name1.
      IF ( wa_info_part-stkzn IS INITIAL ) AND ( wa_info_part-stcd1 IS NOT INITIAL ).

        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = wa_info_part-stcd1
          IMPORTING
            output = cte_text_cliente_cgc.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_info_part-stcd1
          IMPORTING
            output = cte_text_cv_cnpj.

        wa_dados_nf-stcd1 = wa_info_part-stcd1.

      ELSEIF ( wa_info_part-stkzn IS NOT INITIAL ) AND ( wa_info_part-stcd2 IS NOT INITIAL ).

        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = wa_info_part-stcd2
          IMPORTING
            output = cte_text_cliente_cgc.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_info_part-stcd2
          IMPORTING
            output = cte_text_cv_cnpj.

        wa_dados_nf-stcd1 = wa_info_part-stcd2.

      ENDIF.
      IF wa_info_part-txjcd IS NOT INITIAL.
        cte_text_cv_uf = wa_info_part-txjcd+3(2).
      ENDIF.

    ELSE.
      cte_text_cliente_nome = wa_info_c-name1.
      IF ( wa_info_c-stkzn IS INITIAL ) AND ( wa_info_c-stcd1 IS NOT INITIAL ).

        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = wa_info_c-stcd1
          IMPORTING
            output = cte_text_cliente_cgc.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_info_c-stcd1
          IMPORTING
            output = cte_text_cv_cnpj.

        wa_dados_nf-stcd1 = wa_info_c-stcd1.

      ELSEIF ( wa_info_c-stkzn IS NOT INITIAL ) AND ( wa_info_c-stcd2 IS NOT INITIAL ).

        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = wa_info_c-stcd2
          IMPORTING
            output = cte_text_cliente_cgc.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_info_c-stcd2
          IMPORTING
            output = cte_text_cv_cnpj.

        wa_dados_nf-stcd1 = wa_info_c-stcd2.

      ENDIF.
      IF wa_info_c-txjcd IS NOT INITIAL.
        cte_text_cv_uf = wa_info_c-txjcd+3(2).
      ENDIF.

    ENDIF.

  ENDIF.

  wa_dados_nf-modelo = '55'.

  IF wa_dados_nf-dtemissao IS NOT INITIAL.
    cte_text_cv_ano = wa_dados_nf-dtemissao+2(2).
    cte_text_cv_mes = wa_dados_nf-dtemissao+4(2).
  ENDIF.

  IF wa_dados_nf-modelo IS NOT INITIAL.
    WRITE wa_dados_nf-modelo TO cte_text_cv_mod.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cte_text_cv_mod
      IMPORTING
        output = cte_text_cv_mod.
  ENDIF.

  IF wa_dados_nf-serie IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dados_nf-serie
      IMPORTING
        output = cte_text_cv_serie.
  ENDIF.

  IF wa_dados_nf-numero IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dados_nf-numero
      IMPORTING
        output = cte_text_numero.
  ENDIF.

  IF ( wa_dados_nf-numero IS NOT INITIAL ) AND
     ( wa_dados_nf-stcd1  IS NOT INITIAL ) AND
     ( wa_dados_nf-serie  IS NOT INITIAL ) AND
     ( wa_dados_nf-nfe    IS NOT INITIAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dados_nf-numero
      IMPORTING
        output = wa_dados_nf-numero.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dados_nf-serie
      IMPORTING
        output = wa_dados_nf-serie.

    IF wa_dados_nf-serie GE '900' AND
       wa_dados_nf-serie LE '999'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_dados_nf-stcd1
        IMPORTING
          output = cte_text_cv_cnpj.

      wa_dados_nf-stcd1 = cte_text_cv_cnpj.

    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_dados_nf-stcd1
        IMPORTING
          output = cte_text_cv_cnpj.
      wa_dados_nf-stcd1 = cte_text_cv_cnpj.
    ENDIF.

    IF ( wa_dados_nf-pfisica EQ abap_true  ) AND
       ( wa_dados_nf-nfe EQ abap_true      ) AND
       ( strlen( wa_dados_nf-stcd1 ) EQ 14 ) AND
       ( ( wa_dados_nf-serie GE '900' AND wa_dados_nf-serie LE '999' ) OR
         ( wa_dados_nf-serie GE '890' AND wa_dados_nf-serie LE '899' ) ).

      SELECT SINGLE * INTO wa_zib_nfe_dist_ter
        FROM zib_nfe_dist_ter
       WHERE numero     = wa_dados_nf-numero
         AND serie      = wa_dados_nf-serie
         AND forne_cpf  = wa_dados_nf-stcd1+3(11).

    ELSE.

      SELECT SINGLE * INTO wa_zib_nfe_dist_ter
        FROM zib_nfe_dist_ter
       WHERE numero     = wa_dados_nf-numero
         AND serie      = wa_dados_nf-serie
         AND forne_cnpj = wa_dados_nf-stcd1.

    ENDIF.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE * INTO wa_zib_nfe_dist_itm
        FROM zib_nfe_dist_itm
       WHERE chave_nfe = wa_zib_nfe_dist_ter-chave_nfe.

*      CONCATENATE WA_ZIB_NFE_DIST_TER-REGIO
*                  WA_ZIB_NFE_DIST_TER-NFYEAR
*                  WA_ZIB_NFE_DIST_TER-NFMONTH
*                  WA_ZIB_NFE_DIST_TER-FORNE_CNPJ
*                  WA_ZIB_NFE_DIST_TER-MODEL
*                  WA_ZIB_NFE_DIST_TER-SERIE
*                  WA_ZIB_NFE_DIST_TER-NUMERO
*                  WA_ZIB_NFE_DIST_TER-DOCNUM9
*                  WA_ZIB_NFE_DIST_TER-CDV INTO WA_DADOS_NF-CHAVE.

      wa_dados_nf-chave            = wa_zib_nfe_dist_ter-chave_nfe.
      cte_text_cv_uf               = wa_zib_nfe_dist_ter-regio.
      cte_text_cv_ano              = wa_zib_nfe_dist_ter-nfyear.
      cte_text_cv_mes              = wa_zib_nfe_dist_ter-nfmonth.
      wa_dados_nf-modelo           = wa_zib_nfe_dist_ter-model.
      wa_dados_nf-serie            = wa_zib_nfe_dist_ter-serie.
      wa_dados_nf-numero           = wa_zib_nfe_dist_ter-numero.
      wa_dados_nf-docnum9          = wa_zib_nfe_dist_ter-docnum9.
      wa_dados_nf-cdv              = wa_zib_nfe_dist_ter-cdv.
      wa_dados_nf-vl_bc            = wa_zib_nfe_dist_ter-vl_icms_base.
      wa_dados_nf-vl_icms          = wa_zib_nfe_dist_ter-vl_icms_total.
      wa_dados_nf-vl_bc_st         = wa_zib_nfe_dist_ter-vl_icms_st_base.
      wa_dados_nf-vl_st            = wa_zib_nfe_dist_ter-vl_icms_st_total.
      wa_dados_nf-vl_produtos      = wa_zib_nfe_dist_ter-vl_total.
      wa_dados_nf-vl_nota_fiscal   = wa_zib_nfe_dist_ter-vl_total.
      wa_dados_nf-dtemissao        = wa_zib_nfe_dist_ter-dt_emissao.
      wa_dados_nf-cfop             = wa_zib_nfe_dist_itm-prod_cfop && 'AA'.

    ENDIF.
  ELSE.

    sy-subrc = 4.
*====================================Inicio user story 72971 / 07.03.2022 - anderson oenning
    IF ( wa_dados_nf-chave IS NOT INITIAL ).

      "Verifica quantidade caractere.
      zcont_caract = strlen( wa_dados_nf-chave ).
      IF zcont_caract EQ 44.
        SELECT SINGLE * INTO wa_zib_nfe_dist_ter
               FROM zib_nfe_dist_ter
              WHERE chave_nfe = wa_dados_nf-chave.

        SELECT SINGLE * INTO wa_zib_nfe_dist_itm
      FROM zib_nfe_dist_itm
     WHERE chave_nfe = wa_dados_nf-chave.

        wa_dados_nf-chave            = wa_zib_nfe_dist_ter-chave_nfe.
        cte_text_cv_uf               = wa_zib_nfe_dist_ter-regio.
        cte_text_cv_ano              = wa_zib_nfe_dist_ter-nfyear.
        cte_text_cv_mes              = wa_zib_nfe_dist_ter-nfmonth.
        wa_dados_nf-modelo           = wa_zib_nfe_dist_ter-model.
        wa_dados_nf-serie            = wa_zib_nfe_dist_ter-serie.
        wa_dados_nf-numero           = wa_zib_nfe_dist_ter-numero.
        wa_dados_nf-docnum9          = wa_zib_nfe_dist_ter-docnum9.
        wa_dados_nf-cdv              = wa_zib_nfe_dist_ter-cdv.
        wa_dados_nf-vl_bc            = wa_zib_nfe_dist_ter-vl_icms_base.
        wa_dados_nf-vl_icms          = wa_zib_nfe_dist_ter-vl_icms_total.
        wa_dados_nf-vl_bc_st         = wa_zib_nfe_dist_ter-vl_icms_st_base.
        wa_dados_nf-vl_st            = wa_zib_nfe_dist_ter-vl_icms_st_total.
        wa_dados_nf-vl_produtos      = wa_zib_nfe_dist_ter-vl_total.
        wa_dados_nf-vl_nota_fiscal   = wa_zib_nfe_dist_ter-vl_total.
        wa_dados_nf-dtemissao        = wa_zib_nfe_dist_ter-dt_emissao.
        wa_dados_nf-cfop             = wa_zib_nfe_dist_itm-prod_cfop && 'AA'.
        wa_dados_nf-stcd1            = wa_zib_nfe_dist_ter-forne_cnpj.

        wa_dados_nf-vl_bc            = wa_zib_nfe_dist_ter-vl_icms_base.
        wa_dados_nf-vl_icms          = wa_zib_nfe_dist_ter-vl_icms_total.
        wa_dados_nf-vl_bc_st         = wa_zib_nfe_dist_ter-vl_icms_st_base.
        wa_dados_nf-vl_st            = wa_zib_nfe_dist_ter-vl_icms_st_total.
        wa_dados_nf-vl_produtos      = wa_zib_nfe_dist_ter-vl_produtos.
        wa_dados_nf-vl_nota_fiscal   = wa_zib_nfe_dist_ter-vl_total.

      ENDIF.

      "Check nota propria.
      var_cnpj =  wa_dados_nf-chave+6(14).
      SELECT SINGLE * FROM  j_1bbranch INTO @DATA(ws_j_1bbranch) WHERE stcd1 = @var_cnpj.
      IF sy-subrc EQ 0.
        wa_dados_nf-entrad = 'X'. "Nota Própria
      ENDIF.
    ELSE.
      sy-subrc = 4.
    ENDIF.
*====================================Fim user story 72971 / 07.03.2022 - anderson oenning
  ENDIF.
  CLEAR: var_cnpj, ws_j_1bbranch.

  IF wa_dados_nf-entrad EQ abap_true. "Entrada Propria

    p_lifnr_dest = |{ wa_dados_transp-cod_dest_merc ALPHA = IN }|.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(_wl_lfa1)
     WHERE lifnr EQ @p_lifnr_dest.

    IF ( sy-subrc EQ 0 ) AND ( p_lifnr_dest IS NOT INITIAL ).

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = _wl_lfa1-stcd1
        IMPORTING
          output = cte_text_cliente_cgc.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = _wl_lfa1-stcd1
        IMPORTING
          output = cte_text_cv_cnpj.

      wa_dados_nf-stcd1 = cte_text_cv_cnpj.
    ENDIF.
  ENDIF.


  IF ( ( wa_dados_nf-stcd1 IS NOT INITIAL ) AND ( sy-subrc NE 0 ) ) AND wa_dados_nf-chave IS NOT INITIAL OR ( wa_dados_nf-entrad EQ abap_true ) .
    CONCATENATE cte_text_cv_uf       cte_text_cv_ano        cte_text_cv_mes
                wa_dados_nf-stcd1 wa_dados_nf-modelo  cte_text_cv_serie
                cte_text_numero      wa_dados_nf-docnum9 wa_dados_nf-cdv
           INTO wa_dados_nf-chave.
  ENDIF.

ENDFORM.                    " COMPLETA_CHAVE
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_DADOS_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM preenche_dados_nf.

  DATA: vl_max_qtde_aviso  TYPE zlest0109-qtde_aviso,
        vl_idx_item        TYPE i,
        wa_dados_aviso_aux TYPE ty_dados_aviso,
        vl_scacd           TYPE lfa1-scacd.

*  CLEAR: vg_emitente_nfe, vl_scacd.
  wa_dados_nf-partyp  = 'V'.
  wa_dados_nf-cliente = wa_dados_transp-cod_remetente.
*  IF wa_dados_nf-cliente IS NOT INITIAL.
*    SELECT SINGLE scacd FROM lfa1 INTO vl_scacd WHERE lifnr EQ wa_dados_nf-cliente.
*    IF sy-subrc EQ 0.
*      vg_emitente_nfe = abap_true.
*    ENDIF.
*  ENDIF.


  vl_idx_item = 1.
  LOOP AT it_dados_aviso INTO wa_dados_aviso.

    IF vl_idx_item = 1.
      ADD 1 TO vl_idx_item.
      vl_max_qtde_aviso  = wa_dados_aviso-qtde_aviso.
      wa_dados_aviso_aux = wa_dados_aviso.
    ELSE.

      IF wa_dados_aviso-qtde_aviso > vl_max_qtde_aviso.
        vl_max_qtde_aviso  = wa_dados_aviso-qtde_aviso.
        wa_dados_aviso_aux = wa_dados_aviso.
      ENDIF.

    ENDIF.

  ENDLOOP.

  wa_dados_nf-material   = wa_dados_aviso_aux-matnr.
  wa_dados_nf-unidade    = wa_dados_aviso_aux-unidade.
  wa_dados_nf-quantidade = wa_dados_aviso_aux-qtde_aviso.



ENDFORM.                    " PREENCHE_DADOS_NF
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_PARCEIROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM preenche_parceiros  USING    p_wa_saida.



ENDFORM.                    " PREENCHE_PARCEIROS


*&---------------------------------------------------------------------*
*&      Form  PREENCHE_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM preenche_dados  USING  p_wa_saida TYPE ty_saida.

  DATA: lt_split_safra  TYPE TABLE OF char40.

  DATA: vl_idx_placa  TYPE lvc_index,
        vl_centro_in  TYPE werks_d,
        vl_centro_out TYPE werks_d.

  IF vg_view_transp IS NOT INITIAL.

    REFRESH: it_sel_rows, it_zlest0109.

    CALL METHOD wa_alv_aviso_rec->get_selected_rows
      IMPORTING
        et_index_rows = it_sel_rows.

    CHECK NOT it_sel_rows IS INITIAL.

    IF ( lines( it_sel_rows ) NE 1 ).
      MESSAGE 'Selecione uma linha para visualização' TYPE 'I'.
      EXIT.
    ENDIF.

    READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

    READ TABLE it_saida_aviso INTO wa_saida_aviso INDEX wa_sel_rows-index.

    SELECT *
      INTO TABLE it_zlest0109
      FROM zlest0109
     WHERE ebeln = wa_saida_aviso-ebeln
       AND vbeln = wa_saida_aviso-vbeln.


    LOOP AT it_zlest0109 INTO wa_zlest0109.

      CLEAR: wa_dados_aviso.

      MOVE-CORRESPONDING wa_zlest0109 TO wa_dados_aviso.

      SELECT SINGLE *
        INTO wa_mara
        FROM mara
       WHERE matnr = wa_zlest0109-matnr.

      wa_dados_aviso-unidade = wa_mara-meins.

      APPEND wa_dados_aviso TO it_dados_aviso.

    ENDLOOP.

    SELECT SINGLE *
      INTO wa_dados_transp
      FROM zlest0108
     WHERE ebeln = wa_saida_aviso-ebeln
       AND vbeln = wa_saida_aviso-vbeln.


    IF wa_dados_transp-placa_cav IS NOT INITIAL.
      wa_dados_veic-pc_veiculo = wa_dados_transp-placa_cav.
      APPEND wa_dados_veic TO it_dados_veic.
    ENDIF.

    IF wa_dados_transp-placa_car1 IS NOT INITIAL.
      wa_dados_veic-pc_veiculo = wa_dados_transp-placa_car1.
      APPEND wa_dados_veic TO it_dados_veic.
    ENDIF.

    vl_idx_placa = 1.
    LOOP AT it_dados_veic INTO wa_dados_veic.
      PERFORM atualiza_dados_placa USING wa_dados_veic  vl_idx_placa.
      ADD 1 TO  vl_idx_placa.
    ENDLOOP.

    IF wa_dados_transp-placa_car2 IS NOT INITIAL.
      wa_dados_veic-pc_veiculo = wa_dados_transp-placa_car2.
      APPEND wa_dados_veic TO it_dados_veic.
    ENDIF.

    IF wa_dados_transp-placa_car3 IS NOT INITIAL.
      wa_dados_veic-pc_veiculo = wa_dados_transp-placa_car3.
      APPEND wa_dados_veic TO it_dados_veic.
    ENDIF.

    SELECT SINGLE *
      INTO wa_dados_nf
      FROM zlest0110
     WHERE ebeln = wa_saida_aviso-ebeln
       AND vbeln = wa_saida_aviso-vbeln.


  ELSE.


    "------------------------------------------------------------------------------"
    "--\ Itens Aviso
    "------------------------------------------------------------------------------"
    CLEAR: wa_dados_aviso.

    SELECT SINGLE *
      INTO wa_mara
      FROM mara
     WHERE matnr = p_wa_saida-matnr.

    wa_dados_aviso-ebelp   = p_wa_saida-ebelp.
    wa_dados_aviso-matnr   = p_wa_saida-matnr.
    wa_dados_aviso-ebeln   = p_wa_saida-ebeln.
    wa_dados_aviso-lifnr   = p_wa_saida-lifnr.
    wa_dados_aviso-unidade = wa_mara-meins.
    wa_dados_aviso-werks   = p_wa_saida-werks.
    wa_dados_aviso-lgort   = p_wa_saida-lgort.
    wa_dados_aviso-charg   = p_wa_saida-charg.

    APPEND wa_dados_aviso TO it_dados_aviso.

    "------------------------------------------------------------------------------"
    "--\ Parceiros
    "------------------------------------------------------------------------------"
    wa_dados_transp-cod_remetente = wa_saida-lifnr.


    CLEAR: vl_centro_out, vl_centro_in.

    vl_centro_in = p_wa_saida-werks.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro        = vl_centro_in
        tp_centro_out = 'R'
      IMPORTING
        centro_real   = vl_centro_out.

    IF ( sy-subrc EQ 0 ).

      wa_dados_transp-cod_dest_merc = vl_centro_out.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_dados_transp-cod_dest_merc
        IMPORTING
          output = wa_dados_transp-cod_dest_merc.

      wa_dados_transp-cod_loc_entrega = wa_dados_transp-cod_dest_merc.
    ENDIF.

    "------------------------------------------------------------------------------"
    "--\ Modal
    "------------------------------------------------------------------------------"
    CLEAR: lt_split_safra[].
    IF wa_dados_transp-safra_ordem_car IS INITIAL AND
       strlen( wa_saida-charg ) > 3.
      SPLIT wa_saida-charg AT '/' INTO TABLE lt_split_safra.
      IF lines( lt_split_safra ) EQ 2.
        READ TABLE lt_split_safra INTO DATA(lwa_safra) INDEX 2.
        IF strlen( lwa_safra ) EQ 4.
          wa_dados_transp-safra_ordem_car = lwa_safra.
        ENDIF.
      ENDIF.
    ENDIF.

    IF strlen( wa_saida-charg ) = 4 .
      wa_dados_transp-safra_ordem_car = wa_saida-charg.
    ENDIF.

  ENDIF.


ENDFORM.                    " PREENCHE_PARCEIROS


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_DADOS_VEIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_dados_veic .

  IF wa_alv_dados_veic IS NOT INITIAL.

    REFRESH: it_selectedcell.

    CALL METHOD wa_alv_dados_veic->get_selected_cells
      IMPORTING
        et_cell = it_selectedcell.

    LOOP AT it_selectedcell INTO wa_selectedcell WHERE col_id-fieldname = 'PC_VEICULO'.

      READ TABLE it_dados_veic INTO wa_dados_veic INDEX wa_selectedcell-row_id-index.

      PERFORM atualiza_dados_placa USING wa_dados_veic wa_selectedcell-row_id-index.


    ENDLOOP.

    CALL METHOD wa_alv_dados_veic->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.


ENDFORM.                    " ATUALIZA_DADOS_VEIC


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_DADOS_VEIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_dados_placa USING wa_dados_veic TYPE ty_dados_veic
                                p_idx_placa   TYPE lvc_index.

  IF ( wa_dados_veic-pc_veiculo IS NOT INITIAL ).

    CLEAR wa_zlest0002.
    SELECT SINGLE * INTO wa_zlest0002 FROM zlest0002 WHERE pc_veiculo = wa_dados_veic-pc_veiculo.

    IF sy-subrc EQ 0.
      wa_dados_veic-cd_cidade         = wa_zlest0002-cd_cidade.
      wa_dados_veic-cd_uf             = wa_zlest0002-cd_uf.
      wa_dados_veic-cd_renavam        = wa_zlest0002-cd_renavam.
      wa_dados_veic-proprietario      = wa_zlest0002-proprietario.
      wa_dados_veic-tp_veiculo        = wa_zlest0002-tp_veiculo.

      CLEAR wa_lfa1.
      SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = wa_dados_veic-proprietario.
      wa_dados_veic-des_proprietario  = wa_lfa1-name1.

      IF wa_lfa1-stcd1 IS NOT INITIAL.
        wa_dados_veic-cnpj_cpf_prop     = wa_lfa1-stcd1.
      ELSEIF wa_lfa1-stcd2 IS NOT INITIAL.
        wa_dados_veic-cnpj_cpf_prop = wa_lfa1-stcd2.
      ENDIF.

      MODIFY it_dados_veic FROM wa_dados_veic INDEX p_idx_placa .
    ENDIF.

  ENDIF.

ENDFORM.                    " ATUALIZA_DADOS_VEIC


*&---------------------------------------------------------------------*
*&      Form  GERAR_AVISO_RECEBIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_aviso_recebimento .

  DATA: var_answer      TYPE c,
        vl_idx_placas   TYPE i,
        vl_count_pc_cav TYPE i,
        vl_placa_cav    TYPE zlest0108-placa_cav,
        vmessa(80).

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row,
        vg_gerar_migo    TYPE char01,
        it_saida_sel     TYPE TABLE OF ty_saida WITH HEADER LINE,
        it_mov_estq      TYPE TABLE OF zmmt_ee_zgr,
        wa_mov_estq      TYPE zmmt_ee_zgr,
        it_mov_estq_ger  TYPE TABLE OF zmmt_ee_zgr_docs,
        wa_mov_estq_ger  TYPE zmmt_ee_zgr_docs,
        it_active_nfp    TYPE TABLE OF j_1bnfe_active,
        it_doc_nfp       TYPE TABLE OF j_1bnfdoc.

  DATA: vl_msg1       TYPE string,
        vl_msg2       TYPE string,
        vl_msg3       TYPE string,
        vl_msg4       TYPE string,
        vl_msg_exibir TYPE string,
        vl_nf_int     TYPE i,
        vl_serie_int  TYPE i,
        vl_nf_char    TYPE string,
        vl_serie_char TYPE string,
        vl_nf_serie   TYPE string,
        vl_quebra     TYPE string,
        vl_lifnr_dest TYPE lfa1-lifnr.


  IF vg_view_transp IS NOT INITIAL.
    MESSAGE 'Modo de visualização! Operação não permitida!' TYPE  'S'.
    EXIT.
  ENDIF.

  PERFORM preenche_dados_nf.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Confirma geração do aviso de Recebimento?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  REFRESH: it_zlest0109.
  CLEAR: wa_zlest0109, wa_dados_aviso.

  READ TABLE it_dados_aviso INTO wa_dados_aviso INDEX 1.

  wa_dados_transp-ebeln = wa_dados_aviso-ebeln.
  wa_dados_transp-ebelp = wa_dados_aviso-ebelp.
  wa_dados_transp-werks = wa_dados_aviso-werks.
  wa_dados_transp-lgort = wa_dados_aviso-lgort.
  wa_dados_transp-charg = wa_dados_aviso-charg.

  wa_dados_nf-ebeln = wa_dados_aviso-ebeln.
  wa_dados_nf-ebelp = wa_dados_aviso-ebelp.
  wa_dados_nf-werks = wa_dados_aviso-werks.
  wa_dados_nf-lgort = wa_dados_aviso-lgort.
  wa_dados_nf-charg = wa_dados_aviso-charg.

  "-----------------------------------------------------------------------------
  "--- Conversion Exit Alpha Input
  "-----------------------------------------------------------------------------

  PERFORM conversion_exit_alpha.

  "-----------------------------------------------------------------------------
  "--- Validação Itens Aviso
  "-----------------------------------------------------------------------------

  LOOP AT it_dados_aviso INTO wa_dados_aviso.

    MOVE-CORRESPONDING wa_dados_aviso TO wa_zlest0109.
    wa_zlest0109-serie = wa_dados_nf-serie.
    wa_zlest0109-nfnum = wa_dados_nf-numero.

    IF ( wa_dados_aviso-qtde_aviso <= 0 ) OR ( wa_zlest0109-qtde_aviso <= 0 ).
      ROLLBACK WORK.
      MESSAGE 'Existe item(s) sem informar a quantidade do aviso!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_zlest0109-lifnr IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Fornecedor do pedido não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_zlest0109-nfnum IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Número da Nota não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    " 22.06.2022 - RAMON LIMA - 79037 - CS2022000013 - REGRA ZLES0113 -->
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(stvarv_check)
     WHERE name EQ 'ZLEST0113_CFOP_VAL_REM'
       AND low  EQ @wa_dados_nf-cfop(4).


    "Melhoria referente chamado USER STORY 136162 / AOENNING.
    SELECT SINGLE matkl FROM mara INTO @DATA(vg_matkl)
      WHERE matnr EQ @wa_dados_aviso-matnr.
    IF sy-subrc EQ 0.
      SELECT SINGLE *
    FROM tvarvc INTO @DATA(stvarv_matkl)
   WHERE name EQ 'MAGGI_GR_INSUMOS'
     AND low  EQ @vg_matkl.
    ENDIF.

    IF sy-subrc NE 0.
      IF wa_zlest0109-lifnr <> wa_dados_transp-cod_remetente AND stvarv_matkl IS INITIAL.
        ROLLBACK WORK.
        MESSAGE 'Remetente diferente do fornecedor do pedido!' TYPE 'S'.
        RETURN.
      ENDIF.
    ENDIF.
    CLEAR: stvarv_matkl, vg_matkl.
    " 22.06.2022 - RAMON LIMA - 79037 - CS2022000013 - REGRA ZLES0113 <--

    IF wa_zlest0109-serie IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Série da Nota não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    APPEND wa_zlest0109 TO it_zlest0109.

  ENDLOOP.

  "-----------------------------------------------------------------------------
  "--- Validação Parceiros
  "-----------------------------------------------------------------------------
  IF wa_dados_transp-agente_frete IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Emissor do CT-e não informado!' TYPE 'S'.
    RETURN.
  ELSE.
    SELECT SINGLE *
      FROM lfa1
      INTO @DATA(wlfa1)
      WHERE lifnr = @wa_dados_transp-agente_frete.
    IF wlfa1-dlgrp NE '0001'.
      CONCATENATE 'Fornecedor' wa_dados_transp-agente_frete 'não configurado como agente de frete. Solicite ajuste à central de cadastro.' INTO  vmessa SEPARATED BY space.
      MESSAGE vmessa TYPE 'S'.
      RETURN.
    ENDIF.

    IF wlfa1-ktokk = 'ZFIC'.
      vg_tipo_frete = 'CIF'.
    ELSE.
      vg_tipo_frete = 'CPT'.
    ENDIF.
  ENDIF.

  IF wa_dados_transp-cod_remetente IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Remetente da Mercadoria não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_transp-cod_dest_merc IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Destinatário da Mercadoria não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_transp-cod_loc_coleta IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Local de Coleta não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_transp-cod_loc_entrega IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Local de Entrega não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF ( wa_dados_transp-motorista IS INITIAL ) AND ( vg_tipo_frete EQ 'CIF' ).
    ROLLBACK WORK.
    MESSAGE 'Motorista não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  CLEAR: wa_lfa1, wa_kna1,  wa_trolz, wa_tvro.
  SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = wa_dados_transp-cod_loc_coleta.
  SELECT SINGLE * INTO wa_kna1 FROM kna1 WHERE kunnr = wa_dados_transp-cod_loc_entrega.

  SELECT SINGLE *
    INTO wa_trolz
    FROM trolz
   WHERE aland = 'BR'
     AND azone = wa_lfa1-lzone
     AND lland = 'BR'
     AND lzone = wa_kna1-lzone.

  IF ( wa_trolz-route IS INITIAL ).
    ROLLBACK WORK.
    MESSAGE e024(sd) WITH 'Não existe itinerário para ZONA_PC =' wa_lfa1-lzone 'e ZONA LR =' wa_kna1-lzone.
    RETURN.
  ENDIF.

  SELECT SINGLE * INTO wa_tvro FROM tvro WHERE route = wa_trolz-route.
  IF ( wa_tvro-tdiix IS INITIAL ).
    ROLLBACK WORK.
    MESSAGE e024(sd) WITH 'Itinerário' wa_trolz-route 'sem relevância para transporte.' 'Solicite regularização para à logística'.
    RETURN.
  ENDIF.

  "-----------------------------------------------------------------------------
  "--- Validação Dados NF-e
  "-----------------------------------------------------------------------------

  IF wa_dados_nf-modelo IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Modelo da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-numero IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Número da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-serie IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Série da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-dtemissao IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Data Emissão da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-cfop IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'CFOP da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-vl_produtos IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Valor Produtos da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-vl_nota_fiscal IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Valor da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.


  DATA(_inf_transgenia) = abap_false.
  PERFORM f_informa_transgenia USING wa_dados_transp-ebeln CHANGING _inf_transgenia.

  IF _inf_transgenia EQ abap_true.
    IF wa_dados_nf-tp_produto IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Tipo do Produto da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.
  ENDIF.


  IF wa_dados_nf-nfe IS NOT INITIAL.

    IF strlen( wa_dados_nf-chave ) <> 44 .
      ROLLBACK WORK.
      MESSAGE 'Chave da NF-e não possui 44 dígitos!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_dados_nf-entrad EQ abap_true. "Entrada propria

      CLEAR: it_active_nfp[], it_doc_nfp[].

      DATA(_achou_nf_prop) = abap_false.

      SELECT *
        FROM j_1bnfe_active INTO TABLE it_active_nfp
       WHERE stcd1  EQ wa_dados_nf-stcd1
         AND nfnum9 EQ wa_dados_nf-numero
         AND serie  EQ wa_dados_nf-serie.

      IF it_active_nfp[] IS NOT INITIAL.
        SELECT *
          FROM j_1bnfdoc INTO TABLE it_doc_nfp
           FOR ALL ENTRIES IN it_active_nfp
         WHERE docnum  EQ it_active_nfp-docnum.

        LOOP AT it_doc_nfp INTO DATA(wl_doc) WHERE form   IS NOT INITIAL
                                               AND direct EQ '1'
                                               AND candat IS INITIAL.
          _achou_nf_prop = abap_true.
          EXIT.
        ENDLOOP.

      ENDIF.

      IF _achou_nf_prop EQ abap_false.
        ROLLBACK WORK.
        MESSAGE |Documento entrada própria não localizado com a chave: { wa_dados_nf-chave }! | TYPE 'S'.
        RETURN.
      ENDIF.

    ELSE.

      SELECT SINGLE *
        FROM zib_nfe_dist_ter INTO @DATA(wl_zib_nfe_dist_ter_tmp)
       WHERE chave_nfe EQ @wa_dados_nf-chave.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE |XML não recebido para a chave: { wa_dados_nf-chave }! | TYPE 'S'.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zib_nfe_dist_itm INTO @DATA(wl_zib_nfe_dist_itm_tmp)
       WHERE chave_nfe EQ @wa_dados_nf-chave.

      vl_lifnr_dest = |{ wa_dados_transp-cod_dest_merc ALPHA = IN }|.

      SELECT SINGLE *
        FROM lfa1 INTO @DATA(_wl_lfa1_dest)
       WHERE lifnr EQ @vl_lifnr_dest.

      IF ( sy-subrc EQ 0 ) AND ( vl_lifnr_dest IS NOT INITIAL ) AND ( _wl_lfa1_dest-ktokk = 'ZFIC' ).
        IF wl_zib_nfe_dist_ter_tmp-branch NE _wl_lfa1_dest-lifnr+6(4).
          ROLLBACK WORK.
          MESSAGE |Destinatario XML: { wl_zib_nfe_dist_ter_tmp-branch } diferente do informado: { _wl_lfa1_dest-lifnr+6(4) }! | TYPE 'S'.
          RETURN.
        ENDIF.
      ENDIF.

      IF wa_dados_nf-vl_bc NE wl_zib_nfe_dist_ter_tmp-vl_icms_base.
        ROLLBACK WORK.
        MESSAGE |Base de Calculo ICMS divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_icms NE wl_zib_nfe_dist_ter_tmp-vl_icms_total.
        ROLLBACK WORK.
        MESSAGE |Valor ICMS divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_bc_st NE wl_zib_nfe_dist_ter_tmp-vl_icms_st_base.
        ROLLBACK WORK.
        MESSAGE |Base Calculo ST divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_st NE wl_zib_nfe_dist_ter_tmp-vl_icms_st_total.
        ROLLBACK WORK.
        MESSAGE |Valor ICMS ST divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_produtos NE wl_zib_nfe_dist_ter_tmp-vl_total.
        ROLLBACK WORK.
        MESSAGE |Valor Produtos divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_nota_fiscal NE wl_zib_nfe_dist_ter_tmp-vl_total.
        ROLLBACK WORK.
        MESSAGE |Valor Nota Fiscal divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-dtemissao NE wl_zib_nfe_dist_ter_tmp-dt_emissao.
        ROLLBACK WORK.
        MESSAGE |Data Emissão divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      DATA(cfop_tmp) = wl_zib_nfe_dist_itm_tmp-prod_cfop && 'AA'.
      IF wa_dados_nf-cfop NE cfop_tmp.
        ROLLBACK WORK.
        MESSAGE |CFOP divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.


    ENDIF.

  ENDIF.

  CLEAR: wa_zlest0109, wa_dados_aviso.
  READ TABLE it_dados_aviso INTO wa_dados_aviso INDEX 1.

  SELECT SINGLE *
    INTO wa_zlest0109
    FROM zlest0109
   WHERE ebeln = wa_dados_aviso-ebeln
     AND nfnum = wa_dados_nf-numero
     AND serie = wa_dados_nf-serie.

  IF sy-subrc = 0.

    CLEAR: wa_likp.
    SELECT SINGLE *
      INTO wa_likp
      FROM likp
     WHERE vbeln =  wa_zlest0109-vbeln.

    IF sy-subrc = 0.

      ROLLBACK WORK.
      CONCATENATE 'Já existe um aviso de Recebimento(' wa_zlest0109-vbeln
                  ') criado para esse Pedido/NF/Série!'
             INTO vl_msg_exibir SEPARATED BY space.
      MESSAGE vl_msg_exibir TYPE 'S'.
      RETURN.
    ENDIF.

  ENDIF.

  "-----------------------------------------------------------------------------
  "--- Validação Modal
  "-----------------------------------------------------------------------------
  IF ( wa_dados_transp-safra_ordem_car IS INITIAL OR
       wa_dados_transp-nro_ordem_car   IS INITIAL  ) AND vg_tipo_frete = 'CIF'.
    ROLLBACK WORK.
    MESSAGE 'Safra da Ordem de Carregamento é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF vg_tipo_frete EQ 'CIF'.

    SELECT SINGLE *
      INTO wa_zlest0108_aux
      FROM zlest0108
     WHERE safra_ordem_car = wa_dados_transp-safra_ordem_car
       AND nro_ordem_car   = wa_dados_transp-nro_ordem_car.

    IF sy-subrc = 0.

      CLEAR: wa_likp.

      SELECT SINGLE *
        INTO wa_likp
        FROM likp
       WHERE vbeln = wa_zlest0108_aux-vbeln.

      IF sy-subrc = 0.

        vl_msg1 = wa_dados_transp-safra_ordem_car.
        vl_msg2 = wa_dados_transp-nro_ordem_car.
        vl_msg3 = wa_zlest0108_aux-vbeln.

        CONCATENATE 'Ordem de Carregamento de número:' space vl_msg2
                    ', referente a Safra:' space vl_msg1
                    ', já foi utilizada no Aviso de Recebimento: ' vl_msg3
                    '!'
               INTO vl_msg_exibir.

        ROLLBACK WORK.
        MESSAGE vl_msg_exibir TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

  ENDIF.

  IF it_dados_veic[] IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Nenhuma placa encontrada!' TYPE 'S'.
    RETURN.
  ENDIF.

  CLEAR: vl_placa_cav.
  vl_count_pc_cav = 0.
  vl_idx_placas   = 0.
  LOOP AT it_dados_veic INTO wa_dados_veic WHERE pc_veiculo IS NOT INITIAL.
    IF wa_dados_veic-tp_veiculo = '0'.
      ADD 1 TO vl_count_pc_cav.
      vl_placa_cav = wa_dados_veic-pc_veiculo.
    ENDIF.
    ADD 1 TO vl_idx_placas.
  ENDLOOP.

  IF vl_count_pc_cav > 1.
    ROLLBACK WORK.
    MESSAGE 'Determinado mais de um veiculo de tração! Operação não permitida!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF vl_placa_cav IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Não informado o veiculo de tração! Operação não permitida!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF vl_idx_placas > 4.
    ROLLBACK WORK.
    MESSAGE 'Não é permitido informar mais que 4 placas!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF vl_idx_placas <= 0.
    ROLLBACK WORK.
    MESSAGE 'Informar pelo menos uma placa!' TYPE 'S'.
    EXIT.
  ENDIF.

  wa_dados_transp-placa_cav = vl_placa_cav.

  vl_idx_placas = 1.
  LOOP AT it_dados_veic INTO wa_dados_veic WHERE tp_veiculo <> '0'.

    CASE vl_idx_placas.
      WHEN 1.
        wa_dados_transp-placa_car1 = wa_dados_veic-pc_veiculo.
      WHEN 2.
        wa_dados_transp-placa_car2 = wa_dados_veic-pc_veiculo.
      WHEN 3.
        wa_dados_transp-placa_car3 = wa_dados_veic-pc_veiculo.
    ENDCASE.

    ADD 1 TO vl_idx_placas.

  ENDLOOP.

  "-----------------------------------------------------------------------------
  "  Verifica se já tem um Aviso de Recebimento criado para
  "  o Pedido via Interface com o numero NF e Série atual.
  "-----------------------------------------------------------------------------
  CLEAR: wa_mov_estq_ger, wa_mov_estq  , vl_nf_int, vl_serie_int,
         vl_nf_char     , vl_serie_char, vl_nf_serie.

  REFRESH: it_mov_estq_ger, it_mov_estq.

  vl_nf_int      = wa_dados_nf-numero.
  vl_serie_int   = wa_dados_nf-serie.

  vl_nf_char    = vl_nf_int.
  vl_serie_char = vl_serie_int.
  CONCATENATE vl_nf_char '-' vl_serie_char INTO vl_nf_serie.
  CONDENSE vl_nf_serie NO-GAPS.

  IF ( vl_nf_char    IS INITIAL ) OR
     ( vl_serie_char IS INITIAL ) OR
     ( vl_nf_serie   IS INITIAL ) OR
     ( strlen( vl_nf_serie )  < 3 ).
    ROLLBACK WORK.
    MESSAGE 'Houve um erro ao validar a numeração da NF/Série!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: it_mov_estq[].
  SELECT *
    FROM zmmt_ee_zgr INTO TABLE it_mov_estq
   WHERE po_number      EQ wa_dados_aviso-ebeln
     AND ref_doc_no     EQ vl_nf_serie
     AND in_aviso_receb EQ 'S'.

  LOOP AT it_mov_estq INTO wa_mov_estq.
    CLEAR: wa_mov_estq_ger, wa_likp.

    SELECT SINGLE *
      FROM zmmt_ee_zgr_docs INTO wa_mov_estq_ger
     WHERE obj_key EQ wa_mov_estq-obj_key.

    IF ( sy-subrc = 0 ) AND ( wa_mov_estq_ger-av_vbeln IS NOT INITIAL ).

      CLEAR: wa_likp.
      SELECT SINGLE *
        FROM likp INTO wa_likp
       WHERE vbeln     EQ wa_mov_estq_ger-av_vbeln
         AND spe_loekz EQ ''.

      IF sy-subrc = 0.
        vl_msg2   = vl_nf_serie.
        vl_msg3   = wa_dados_aviso-ebeln.
        vl_msg4   = wa_mov_estq-obj_key.

        CONCATENATE 'Já existe um processo em andamento para criação de Aviso. Rec. c/ os dados seguir: '
                    'Pedido:'      vl_msg3 '/'
                    'NF-Série:'    vl_msg2 '/'
                    'Chave Ref.:'  vl_msg4
               INTO vl_msg_exibir SEPARATED BY space.

        ROLLBACK WORK.
        MESSAGE vl_msg_exibir TYPE 'S'.
        RETURN.
      ENDIF.

    ENDIF.
  ENDLOOP.

  CLEAR: wa_mov_estq_ger, wa_mov_estq  , vl_nf_int, vl_serie_int,
         vl_nf_char     , vl_serie_char, vl_nf_serie.

  REFRESH: it_mov_estq_ger, it_mov_estq.


  SELECT SINGLE * INTO @DATA(wa_ekko)
    FROM ekko
   WHERE ebeln EQ @wa_dados_transp-ebeln.

  IF _inf_transgenia EQ abap_true.
    TRY .
        IF wa_dados_nf-tp_produto IS NOT INITIAL.
          zcl_deposito=>zif_deposito~get_instance(
            )->get_deposito_material_filial(
            EXPORTING
              i_matnr          = wa_dados_nf-material    " Nº do material
              i_tp_produto     = wa_dados_nf-tp_produto    " Tipo de Produto
              i_bukrs          = wa_ekko-bukrs " Empresa
              i_centro_a_fixar = wa_dados_transp-werks
            IMPORTING
              e_lgort          = wa_dados_transp-lgort ).
        ELSE.
          wa_dados_transp-lgort = 'ARMZ'.
        ENDIF.
      CATCH zcx_deposito.    "
        wa_dados_transp-lgort = 'ARMZ'.
    ENDTRY.
  ENDIF.

  LOOP AT it_zlest0109 ASSIGNING FIELD-SYMBOL(<fs_0109>).
    <fs_0109>-lgort = wa_dados_transp-lgort.
  ENDLOOP.

  wa_dados_nf-lgort = wa_dados_transp-lgort.

  CLEAR: wa_mov_estq_ger.

  CALL FUNCTION 'Z_MM_CRIAR_AVISO'
    EXPORTING
      wa_zlest0108       = wa_dados_transp
      wa_zlest0110       = wa_dados_nf
      data_aviso         = sy-datum
      gerar_apenas_aviso = 'X'
      frete_ent_terc     = 'X'
    IMPORTING
      doc_gerados        = wa_mov_estq_ger
    TABLES
      it_zlest0109       = it_zlest0109
    EXCEPTIONS
      error              = 1
      OTHERS             = 2.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  IF wa_mov_estq_ger IS NOT INITIAL.
    LEAVE TO SCREEN 0.
  ENDIF.


ENDFORM.                    " GERAR_AVISO_RECEBIMENTO
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_CAMPOS_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_campos_nf .

  DATA: it_doc_model TYPE TABLE OF dd07v WITH KEY domvalue_l,
        wa_doc_text  TYPE dd07v,
        wa_j_1bagt   TYPE j_1bagt.

  CLEAR: cte_text_modelo,
         cte_text_cliente_nome,
         cte_text_cliente_cgc,
         cte_text_cfop,
         cte_text_numero,
         cte_text_cv_uf,
         cte_text_cv_ano,
         cte_text_cv_mes,
         cte_text_cv_cnpj,
         cte_text_cv_mod,
         cte_text_cv_serie,
         cte_text_produto.

  IF wa_dados_nf-cfop IS NOT INITIAL.

    SELECT SINGLE * INTO wa_j_1bagt
      FROM j_1bagt
     WHERE spras EQ sy-langu
       AND cfop  EQ wa_dados_nf-cfop.

    IF sy-subrc IS INITIAL.
      cte_text_cfop = wa_j_1bagt-cfotxt.
    ENDIF.

  ENDIF.

  IF ( wa_dados_nf-material IS NOT INITIAL ) .
    SELECT SINGLE maktx INTO cte_text_produto
      FROM makt
     WHERE matnr EQ wa_dados_nf-material
       AND spras EQ sy-langu.
  ENDIF.

  IF ( wa_dados_nf-cliente IS NOT INITIAL ). "AND ( ( CTE_ALTERANDO EQ 'A' ) OR ( CTE_ALTERANDO EQ 'I' ) ).

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_dados_nf-cliente
        p_partype    = wa_dados_nf-partyp
      CHANGING
        wa_info_part = wa_info_k
        wa_info_c    = wa_info_c.

    wa_dados_nf-stcd1   = wa_info_k-stcd1.
    wa_dados_nf-stcd2   = wa_info_k-stcd2.
    wa_dados_nf-name1   = wa_info_k-name1.
    wa_dados_nf-pfisica = wa_info_k-stkzn.

  ENDIF.

  IF ( wa_dados_nf-pfisica IS INITIAL ) AND ( wa_dados_nf-stcd1 IS NOT INITIAL ).
    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
      EXPORTING
        input  = wa_dados_nf-stcd1
      IMPORTING
        output = cte_text_cliente_cgc.
  ELSEIF ( wa_dados_nf-pfisica IS NOT INITIAL ) AND ( wa_dados_nf-stcd2 IS NOT INITIAL ).
    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = wa_dados_nf-stcd2
      IMPORTING
        output = cte_text_cliente_cgc.
  ENDIF.

  cte_text_cliente_nome = wa_dados_nf-name1.

  IF wa_dados_nf-modelo IS NOT INITIAL.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = 'J_1BMODEL'
        state         = 'A'
        langu         = sy-langu
      TABLES
        dd07v_tab     = it_doc_model
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    READ TABLE it_doc_model INTO wa_doc_text WITH KEY domvalue_l = wa_dados_nf-modelo.
    cte_text_modelo = wa_doc_text-ddtext.

    IF wa_dados_nf-modelo EQ '55'.
      wa_dados_nf-nfe = 'X'.
    ENDIF.
  ENDIF.



  IF wa_dados_nf-nfe IS NOT INITIAL.
    PERFORM completa_chave.
    LOOP AT SCREEN.
      IF ( screen-name EQ 'WA_DADOS_NF-NFNUM9' ) OR ( screen-name EQ 'WA_DADOS_NF-CDV' )
        OR ( screen-name EQ 'WA_DADOS_NF-CHAVE' ).
        screen-output = '1'.
        screen-input  = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSE.
    CLEAR: wa_dados_nf-docnum9,
           wa_dados_nf-cdv,
           wa_dados_nf-chave.

    LOOP AT SCREEN.
      IF ( screen-name EQ 'WA_DADOS_NF-NFNUM9' ) OR ( screen-name EQ 'WA_DADOS_NF-CDV' )
        OR ( screen-name EQ 'WA_DADOS_NF-CHAVE' ).
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.


*====================================inicio user story 72971 / 07.03.2022 - anderson oenning
  IF wa_dados_transp-cod_remetente IS NOT INITIAL.
    SELECT SINGLE * FROM  lfa1 INTO @DATA(w_lfa1) WHERE lifnr EQ @wa_dados_transp-cod_remetente.
    IF w_lfa1-scacd EQ '9999'.
      PERFORM completa_chave.
*      wa_dados_nf-nfe = 'X'.
      LOOP AT SCREEN.
        IF ( screen-name EQ 'WA_DADOS_NF-SERIE' )       OR ( screen-name EQ 'WA_DADOS_NF-CFOP' )
        OR ( screen-name EQ 'WA_DADOS_NF-DTEMISSAO' )   OR ( screen-name EQ 'WA_DADOS_NF-MATERIAL' )
        OR ( screen-name EQ 'WA_DADOS_NF-NUMERO' )      OR ( screen-name EQ 'WA_DADOS_NF-CHAVE' )
        OR ( screen-name EQ 'WA_DADOS_NF-VL_BC' )       OR ( screen-name EQ 'WA_DADOS_NF-VL_ICMS' )
        OR ( screen-name EQ 'WA_DADOS_NF-VL_BC_ST' )    OR ( screen-name EQ 'WA_DADOS_NF-VL_ST' )
        OR ( screen-name EQ 'WA_DADOS_NF-VL_PRODUTOS' ) OR ( screen-name EQ 'WA_DADOS_NF-VL_NOTA_FISCAL' )

        OR ( screen-name EQ 'WA_DADOS_NF-MODELO' ) OR ( screen-name EQ 'CTE_TEXT_CV_CNPJ' )
        OR ( screen-name EQ 'WA_DADOS_NF-DOCNUM9' ).

          screen-output = 1.
          screen-input  = 0.
          MODIFY SCREEN.
        ENDIF.

        IF ( screen-name EQ 'WA_DADOS_NF-CHAVE' ).
          screen-output = 1.
          screen-input  = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSE.
      IF w_lfa1-scacd EQ '8888'.
        wa_dados_nf-modelo = '01'.
        FREE: it_doc_model.
        CLEAR: wa_doc_text.
        IF wa_dados_nf-modelo IS NOT INITIAL.

          CALL FUNCTION 'DDIF_DOMA_GET'
            EXPORTING
              name          = 'J_1BMODEL'
              state         = 'A'
              langu         = sy-langu
            TABLES
              dd07v_tab     = it_doc_model
            EXCEPTIONS
              illegal_input = 1
              OTHERS        = 2.

          READ TABLE it_doc_model INTO wa_doc_text WITH KEY domvalue_l = wa_dados_nf-modelo.
          cte_text_modelo = wa_doc_text-ddtext.
        ENDIF.



        LOOP AT SCREEN.
          IF ( screen-name EQ 'WA_DADOS_NF-SERIE' )       OR ( screen-name EQ 'WA_DADOS_NF-CFOP' )
          OR ( screen-name EQ 'WA_DADOS_NF-DTEMISSAO' )   OR ( screen-name EQ 'WA_DADOS_NF-MATERIAL' )
          OR ( screen-name EQ 'WA_DADOS_NF-NUMERO' )      OR ( screen-name EQ 'WA_DADOS_NF-CHAVE' )
          OR ( screen-name EQ 'WA_DADOS_NF-VL_BC' )       OR ( screen-name EQ 'WA_DADOS_NF-VL_ICMS' )
          OR ( screen-name EQ 'WA_DADOS_NF-VL_BC_ST' )    OR ( screen-name EQ 'WA_DADOS_NF-VL_ST' )
          OR ( screen-name EQ 'WA_DADOS_NF-VL_PRODUTOS' ) OR ( screen-name EQ 'WA_DADOS_NF-VL_NOTA_FISCAL' )
          OR ( screen-name EQ 'WA_DADOS_NF-QUANTIDADE' ).

            screen-output = 1.
            screen-input  = 1.
            MODIFY SCREEN.
          ENDIF.

          IF ( screen-name EQ 'WA_DADOS_NF-CHAVE' )      OR ( screen-name EQ 'WA_DADOS_NF-MODELO')
          OR ( screen-name EQ 'WA_DADOS_NF-NFE' )        OR ( screen-name EQ 'WA_DADOS_NF-ENTRAD' )
          OR ( screen-name EQ 'CTE_TEXT_CV_UF'  )         OR ( screen-name EQ 'CTE_TEXT_CV_ANO'   )
          OR ( screen-name EQ 'CTE_TEXT_CV_MES' )         OR ( screen-name EQ 'CTE_TEXT_CV_CNPJ'  )
          OR ( screen-name EQ 'CTE_TEXT_CV_MOD' )         OR ( screen-name EQ 'CTE_TEXT_CV_SERIE' )
          OR ( screen-name EQ 'CTE_TEXT_NUMERO' )         OR ( screen-name EQ 'WA_DADOS_NF-CDV'   )
          OR ( screen-name EQ 'WA_DADOS_NF-DOCNUM9'   ).

            screen-output = 1.
            screen-input  = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR: w_lfa1.


  IF r_atrib IS NOT INITIAL.

    LOOP AT SCREEN.
      IF ( screen-group2 EQ 'ATR' ).
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " ATUALIZA_CAMPOS_NF
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_CAMPOS_PARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_campos_parc .

  CLEAR:  wa_xnome_emit,
          wa_cnpj_emit,
          wa_ie_emit,

          wa_xnome_rem,
          wa_cnpj_rem,
          wa_ie_rem,

          wa_xnome_dest,
          wa_cnpj_dest,
          wa_ie_dest,

          wa_xnome_cole,
          wa_cnpj_cole,
          wa_ie_cole,

          wa_xnome_entrega,
          wa_cnpj_entrega,
          wa_ie_entrega.


*  IF vg_view_transp IS INITIAL.
*    CLEAR: wa_dados_nf,
*           wa_dados_aviso. "BUG IMPEDITIVO 75802 - Anderson Oenning
*  ENDIF.

  IF ( wa_dados_transp-agente_frete IS NOT INITIAL ).

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_dados_transp-agente_frete
        p_partype    = 'V'
      CHANGING
        wa_info_part = wa_info_k
        wa_info_c    = wa_info_c.

    IF wa_info_k-dlgrp IS INITIAL.
      CLEAR: wa_xnome_emit, wa_cnpj_emit, wa_ie_emit, wa_dados_transp-agente_frete.
      MESSAGE 'Dados do Emissor do CT-e inválidos!' TYPE 'S'.
      EXIT.
    ENDIF.

    wa_xnome_emit   =  wa_info_k-name1.
    wa_cnpj_emit    =  wa_info_k-stcd1.
    wa_ie_emit      =  wa_info_k-stcd3.

  ENDIF.

  IF ( wa_dados_transp-cod_remetente IS NOT INITIAL ).

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_dados_transp-cod_remetente
        p_partype    = 'V'
      CHANGING
        wa_info_part = wa_info_k
        wa_info_c    = wa_info_c.

    wa_xnome_rem   =  wa_info_k-name1.
    wa_cnpj_rem    =  wa_info_k-stcd1.
    wa_ie_rem      =  wa_info_k-stcd3.

  ENDIF.

  IF ( wa_dados_transp-cod_dest_merc IS NOT INITIAL ).

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_dados_transp-cod_dest_merc
        p_partype    = 'V'
      CHANGING
        wa_info_part = wa_info_k
        wa_info_c    = wa_info_c.

    wa_xnome_dest   =  wa_info_k-name1.
    wa_cnpj_dest    =  wa_info_k-stcd1.
    wa_ie_dest      =  wa_info_k-stcd3.

  ENDIF.

  IF ( wa_dados_transp-cod_loc_coleta IS NOT INITIAL ).

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_dados_transp-cod_loc_coleta
        p_partype    = 'V'
      CHANGING
        wa_info_part = wa_info_k
        wa_info_c    = wa_info_c.

    wa_xnome_cole   =  wa_info_k-name1.
    wa_cnpj_cole    =  wa_info_k-stcd1.
    wa_ie_cole      =  wa_info_k-stcd3.

  ENDIF.

  IF ( wa_dados_transp-cod_loc_entrega IS NOT INITIAL ).

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_dados_transp-cod_loc_entrega
        p_partype    = 'C'
      CHANGING
        wa_info_part = wa_info_k
        wa_info_c    = wa_info_c.

    wa_xnome_entrega   =  wa_info_c-name1.
    wa_cnpj_entrega    =  wa_info_c-stcd1.
    wa_ie_entrega      =  wa_info_c-stcd3.

  ENDIF.

ENDFORM.                    " ATUALIZA_CAMPOS_PARC
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_CAMPOS_MODAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_campos_modal .

  DATA: obj_ws_ord_car TYPE REF TO zcl_webservice_ord_car,
        obj_ord_car    TYPE REF TO zcl_ordem_car.

  DATA: vl_safra        TYPE string,
        vl_nr_ordem     TYPE string,
        vl_mensagem_ret TYPE string,
        vl_msg_exibir   TYPE string,
        vl_idx_placa    TYPE lvc_index.

  CLEAR: wa_xnome_moto,wa_cpf_moto.

  SELECT SINGLE *
     FROM lfa1
     INTO @DATA(wlfa1)
     WHERE lifnr = @wa_dados_transp-agente_frete.

  IF wlfa1-ktokk = 'ZFIC'.
    vg_tipo_frete = 'CIF'.
  ELSE.
    vg_tipo_frete = 'CPT'.
  ENDIF.


  "Só habilita os campos após limpar os dados do Modal e não estar em modo de visualização.
  IF r_atrib IS INITIAL.

    IF ( wa_dados_transp-safra_ordem_car IS NOT INITIAL ) AND
       ( wa_dados_transp-nro_ordem_car   IS NOT INITIAL ) AND
       ( it_dados_veic[] IS INITIAL ) AND
       ( vg_view_transp IS INITIAL )  AND
       ( vg_tipo_frete NE 'CPT' ).

      REFRESH: it_dados_veic.
      CLEAR: wa_dados_transp-motorista.

      vl_safra    = wa_dados_transp-safra_ordem_car.
      vl_nr_ordem = wa_dados_transp-nro_ordem_car.

      FREE: obj_ws_ord_car, obj_ord_car.

      CREATE OBJECT obj_ws_ord_car.
      CREATE OBJECT obj_ord_car.

      obj_ws_ord_car->buscar_transporte( EXPORTING i_safra     = vl_safra
                                                   i_nr_ordem  = vl_nr_ordem
                                                   i_filial    = p_werks-low "IR246503 - Correção busca OC #184471 - BG
                                         RECEIVING e_ordem_car = obj_ord_car ).

      IF obj_ord_car IS NOT INITIAL.

        vl_mensagem_ret = obj_ord_car->get_mensagem_ret( ).

        IF vl_mensagem_ret = 'Sucesso'.

          wa_dados_veic-pc_veiculo = obj_ord_car->get_placa_cav( ).
          APPEND wa_dados_veic TO it_dados_veic.

          wa_dados_veic-pc_veiculo = obj_ord_car->get_placa1( ).
          APPEND wa_dados_veic TO it_dados_veic.

          wa_dados_veic-pc_veiculo = obj_ord_car->get_placa2( ).
          APPEND wa_dados_veic TO it_dados_veic.

          wa_dados_veic-pc_veiculo = obj_ord_car->get_placa3( ).
          APPEND wa_dados_veic TO it_dados_veic.

          wa_dados_transp-motorista = obj_ord_car->get_motorista( ).
          wa_dados_transp-id_ordem  = obj_ord_car->at_zsdt0001od-id_ordem.

          vl_idx_placa = 1.
          LOOP AT it_dados_veic INTO wa_dados_veic.
            PERFORM atualiza_dados_placa USING wa_dados_veic  vl_idx_placa.
            ADD 1 TO  vl_idx_placa.
          ENDLOOP.

          IF ( it_dados_veic[] IS INITIAL ) OR ( wa_dados_transp-motorista IS INITIAL ).
            REFRESH: it_dados_veic.
            CLEAR: wa_dados_transp-motorista.
          ENDIF.

        ELSE.
          IF ( it_dados_veic[] IS INITIAL ) OR ( wa_dados_transp-motorista IS INITIAL ).
            REFRESH: it_dados_veic.
            CLEAR: wa_dados_transp-motorista.
          ENDIF.

          MESSAGE vl_mensagem_ret TYPE 'S'.
          RETURN.
        ENDIF.

      ELSE.
        IF ( it_dados_veic[] IS INITIAL ) OR ( wa_dados_transp-motorista IS INITIAL ).
          REFRESH: it_dados_veic.
          CLEAR: wa_dados_transp-motorista.
        ENDIF.
        MESSAGE 'Não foi possível fazer a busca dos dados do Modal!' TYPE 'S'.
        RETURN.
      ENDIF.

    ENDIF.


    IF ( wa_dados_transp-motorista IS NOT INITIAL ).

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro   = wa_dados_transp-motorista
          p_partype    = 'V'
        CHANGING
          wa_info_part = wa_info_k
          wa_info_c    = wa_info_c.

      IF wa_info_k-ktokk <> 'ZMOT'.
        CLEAR: vl_msg_exibir.
        CONCATENATE 'Dados do cadastro do Motorista(' wa_dados_transp-motorista ') estão inválidos!'
               INTO vl_msg_exibir SEPARATED BY space.

        CLEAR: wa_xnome_moto,wa_cpf_moto,wa_dados_transp-motorista.
        REFRESH: it_dados_veic.
        MESSAGE vl_msg_exibir TYPE 'S'.
      ELSE.
        wa_xnome_moto = wa_info_k-name1.
        wa_cpf_moto   = wa_info_k-stcd2.
      ENDIF.

    ENDIF.

    IF ( it_dados_veic[] IS INITIAL ) OR ( wa_dados_transp-motorista IS INITIAL AND vg_tipo_frete = 'CIF' ).
      REFRESH: it_dados_veic.
      CLEAR: wa_dados_transp-motorista.
    ENDIF.

    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'WA_DADOS_TRANSP-SAFRA_ORDEM_CAR'    OR
             'WA_DADOS_TRANSP-NRO_ORDEM_CAR'.
          IF vg_tipo_frete EQ 'CPT' .
            screen-input  = '0'.
          ELSEIF ( it_dados_veic[] IS INITIAL ) AND ( vg_view_transp IS INITIAL ).
            screen-input  = '1'.
          ELSE.
            screen-input  = '0'.
          ENDIF.
          MODIFY SCREEN.
        WHEN 'WA_DADOS_TRANSP-MOTORISTA'.
          IF vg_tipo_frete EQ 'CPT' .
            screen-input  = '1'.
          ELSE.
            screen-input  = '0'.
          ENDIF.
          MODIFY SCREEN.
      ENDCASE.

    ENDLOOP.

  ELSE.

*---------------------------------------------------------------------
*---------------------------------------------------------------------
* Tratativa de tela para Atribuição
*---------------------------------------------------------------------
*---------------------------------------------------------------------
    IF ( wa_dados_transp-safra_ordem_car IS NOT INITIAL ) AND
       ( wa_dados_transp-nro_ordem_car   IS NOT INITIAL ) AND
       ( it_dados_veic[] IS INITIAL ) AND
       ( vg_view_transp IS INITIAL )."  AND
*       ( vg_tipo_frete NE 'CPT' ).

      REFRESH: it_dados_veic.
      CLEAR: wa_dados_transp-motorista.

      vl_safra    = wa_dados_transp-safra_ordem_car.
      vl_nr_ordem = wa_dados_transp-nro_ordem_car.

      FREE: obj_ws_ord_car, obj_ord_car.

      CREATE OBJECT obj_ws_ord_car.
      CREATE OBJECT obj_ord_car.

      obj_ws_ord_car->buscar_transporte( EXPORTING i_safra     = vl_safra
                                                   i_nr_ordem  = vl_nr_ordem
                                                   i_filial    = p_werks-low "IR246503 - Correção busca OC #184471 - BG
                                         RECEIVING e_ordem_car = obj_ord_car ).

      IF obj_ord_car IS NOT INITIAL.

        vl_mensagem_ret = obj_ord_car->get_mensagem_ret( ).

        IF vl_mensagem_ret = 'Sucesso'.

          wa_dados_veic-pc_veiculo = obj_ord_car->get_placa_cav( ).
          APPEND wa_dados_veic TO it_dados_veic.

          wa_dados_veic-pc_veiculo = obj_ord_car->get_placa1( ).
          APPEND wa_dados_veic TO it_dados_veic.

          wa_dados_veic-pc_veiculo = obj_ord_car->get_placa2( ).
          APPEND wa_dados_veic TO it_dados_veic.

          wa_dados_transp-motorista = obj_ord_car->get_motorista( ).
          wa_dados_transp-id_ordem  = obj_ord_car->at_zsdt0001od-id_ordem.

          vl_idx_placa = 1.
          LOOP AT it_dados_veic INTO wa_dados_veic.
            PERFORM atualiza_dados_placa USING wa_dados_veic  vl_idx_placa.
            ADD 1 TO  vl_idx_placa.
          ENDLOOP.

          IF ( it_dados_veic[] IS INITIAL ) OR ( wa_dados_transp-motorista IS INITIAL ).
            REFRESH: it_dados_veic.
            CLEAR: wa_dados_transp-motorista.
          ENDIF.

        ELSE.
          IF ( it_dados_veic[] IS INITIAL ) OR ( wa_dados_transp-motorista IS INITIAL ).
            REFRESH: it_dados_veic.
            CLEAR: wa_dados_transp-motorista.
          ENDIF.

          MESSAGE vl_mensagem_ret TYPE 'S'.
          RETURN.
        ENDIF.

      ELSE.
        IF ( it_dados_veic[] IS INITIAL ) OR ( wa_dados_transp-motorista IS INITIAL ).
          REFRESH: it_dados_veic.
          CLEAR: wa_dados_transp-motorista.
        ENDIF.
        MESSAGE 'Não foi possível fazer a busca dos dados do Modal!' TYPE 'S'.
        RETURN.
      ENDIF.

    ENDIF.

    IF ( wa_dados_transp-motorista IS NOT INITIAL ).

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro   = wa_dados_transp-motorista
          p_partype    = 'V'
        CHANGING
          wa_info_part = wa_info_k
          wa_info_c    = wa_info_c.

      IF wa_info_k-ktokk <> 'ZMOT'.
        CLEAR: vl_msg_exibir.
        CONCATENATE 'Dados do cadastro do Motorista(' wa_dados_transp-motorista ') estão inválidos!'
               INTO vl_msg_exibir SEPARATED BY space.

        CLEAR: wa_xnome_moto,wa_cpf_moto,wa_dados_transp-motorista.
        REFRESH: it_dados_veic.
        MESSAGE vl_msg_exibir TYPE 'S'.
      ELSE.
        wa_xnome_moto = wa_info_k-name1.
        wa_cpf_moto   = wa_info_k-stcd2.
      ENDIF.

    ENDIF.

    IF ( ( it_dados_veic[] IS INITIAL ) OR ( wa_dados_transp-motorista IS INITIAL ) ) AND vg_tipo_frete = 'CIF'.
      REFRESH: it_dados_veic.
      CLEAR: wa_dados_transp-motorista.
    ENDIF.

    LOOP AT SCREEN.
*      CASE screen-name.
      IF screen-name = 'WA_DADOS_TRANSP-SAFRA_ORDEM_CAR'    OR
        screen-name =  'WA_DADOS_TRANSP-NRO_ORDEM_CAR' OR
         screen-name =  'WA_DADOS_TRANSP-MOTORISTA'.
*          IF VG_TIPO_FRETE EQ 'CPT' .
*            SCREEN-INPUT  = '0'.
*          ELSE
        IF ( it_dados_veic[] IS INITIAL ) AND ( vg_view_transp IS INITIAL ).
          screen-input  = '1'.
        ELSE.
          screen-input  = '0'.
        ENDIF.
        MODIFY SCREEN.
*        WHEN 'WA_DADOS_TRANSP-MOTORISTA'.
*          IF vg_tipo_frete EQ 'CPT' .
*            screen-input  = '1'.
*          ELSE.
*            screen-input  = '0'.
*          ENDIF.
*          MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDIF.


ENDFORM.                    " ATUALIZA_CAMPOS_MODAL

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_AVISO_REC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_aviso_rec .

  DATA: tabix      TYPE sy-tabix,
        v_cont_fre TYPE i,
        v_cd_uf    TYPE zlest0002-cd_uf,
        v_agregado TYPE zlest0002-agregado.

  FIELD-SYMBOLS: <saida> TYPE ty_saida_aviso.
  UNASSIGN <saida>.

  REFRESH: it_saida_aviso, it_zlest0108_aux.

  CHECK wa_alv_ped IS NOT INITIAL.
  CHECK wa_alv_aviso_rec IS NOT INITIAL.

  CLEAR: it_sel_rows[], wa_sel_rows, wa_saida.

  CALL METHOD wa_alv_ped->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CALL METHOD wa_alv_aviso_rec->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CHECK NOT it_sel_rows IS INITIAL.

  IF ( lines( it_sel_rows ) NE 1 ).
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida INTO wa_saida INDEX wa_sel_rows-index.

  PERFORM atualiza_aviso USING wa_saida.


ENDFORM.                    " ATUALIZA_AVISO_REC


*&---------------------------------------------------------------------*
*&      Form  AVISO_PED_SELECTED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM aviso_ped_selected .

  DATA: tabix       TYPE sy-tabix,
        v_cont_fre  TYPE i,
        v_cd_uf     TYPE zlest0002-cd_uf,
        v_agregado  TYPE zlest0002-agregado,
        l_eindt_ini TYPE char10,
        l_eindt_fim TYPE char10.

  FIELD-SYMBOLS: <saida> TYPE ty_saida_aviso.
  UNASSIGN <saida>.

*-CS2022001149-29.03.2023-#103469-JT-inicio
  FREE:  t_dynpfields.
  w_dynpfields-fieldname = 'VG_EINDT_INI'.
  APPEND w_dynpfields   TO t_dynpfields.
  w_dynpfields-fieldname = 'VG_EINDT_FIM'.
  APPEND w_dynpfields   TO t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields
    EXCEPTIONS
      OTHERS             = 0.

  TRY.
      l_eindt_ini = t_dynpfields[ fieldname = 'VG_EINDT_INI' ]-fieldvalue.
    CATCH cx_sy_itab_line_not_found INTO DATA(l_error).
  ENDTRY.

  TRY.
      l_eindt_fim = t_dynpfields[ fieldname = 'VG_EINDT_FIM' ]-fieldvalue.
    CATCH cx_sy_itab_line_not_found INTO l_error.
  ENDTRY.

  vg_eindt_ini = l_eindt_ini+6(4) && l_eindt_ini+3(2) && l_eindt_ini(2).
  vg_eindt_fim = l_eindt_fim+6(4) && l_eindt_fim+3(2) && l_eindt_fim(2).

  IF   vg_eindt_ini       IS INITIAL AND vg_eindt_fim IS NOT INITIAL.
    MESSAGE s024(sd) WITH 'Intervalo de Datas Incorreto!' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF vg_eindt_ini > vg_eindt_fim AND vg_eindt_fim IS NOT INITIAL.
    MESSAGE s024(sd) WITH 'Intervalo de Datas Incorreto!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*-CS2022001149-29.03.2023-#103469-JT-fim

  REFRESH: it_saida_aviso, it_zlest0108_aux.

  CHECK wa_alv_ped IS NOT INITIAL.
  CHECK wa_alv_aviso_rec IS NOT INITIAL.

  CLEAR: it_sel_rows[], wa_sel_rows, wa_saida.

  CALL METHOD wa_alv_ped->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CALL METHOD wa_alv_aviso_rec->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CHECK NOT it_sel_rows IS INITIAL.

  IF ( lines( it_sel_rows ) NE 1 ).
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida INTO wa_saida INDEX wa_sel_rows-index.

  PERFORM atualiza_aviso USING wa_saida.


ENDFORM.                    " ATUALIZA_AVISO_REC


*&---------------------------------------------------------------------*
*&      Form  GERAR_DOC_TRANSP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_doc_transp .

  CLEAR: wa_zlest0108.

  CALL METHOD wa_alv_aviso_rec->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK NOT it_sel_rows IS INITIAL.

  IF ( lines( it_sel_rows ) NE 1 ).
    MESSAGE 'Selecione apenas um Aviso de Recebimento!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida_aviso INTO wa_saida_aviso INDEX wa_sel_rows-index.


  SELECT SINGLE *
    FROM zlest0108
    INTO wa_zlest0108
   WHERE ebeln = wa_saida_aviso-ebeln
     AND vbeln = wa_saida_aviso-vbeln.

  IF wa_zlest0108-fknum GT 0.
    MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wa_zlest0108-fat_contingencia_ecc = abap_true.
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_user_fat)
     WHERE name = 'FAT_CONTINGENCIA_GOLIVE_US'
       AND low  = @sy-uname.

    IF sy-subrc NE 0.
      MESSAGE 'Faturamento Contingencia ECC! Operação não permitida!' TYPE 'I'.
      EXIT.
    ENDIF.

  ENDIF.

  UPDATE zlest0108 SET st_proc = wa_saida_aviso-st_proc
    WHERE ebeln = wa_saida_aviso-ebeln
      AND vbeln = wa_saida_aviso-vbeln.

  CLEAR: wl_erro, v_tknum.

  PERFORM f_gerar_vt USING wa_saida_aviso wa_zlest0108  CHANGING wl_erro.

  IF wl_erro EQ 'N' AND v_tknum IS NOT INITIAL.

    wa_saida_aviso-transp = v_tknum.

    MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING transp.

    CALL METHOD wa_alv_aviso_rec->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    UPDATE zlest0108 SET doc_transp = wa_saida_aviso-transp
                         st_proc     = '04' " doc transporte
      WHERE ebeln = wa_saida_aviso-ebeln
        AND vbeln = wa_saida_aviso-vbeln.

    wa_saida_aviso-st_proc = '04'.

    MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING st_proc.

    DATA(lva_data_mov_custo) = sy-datum.

    IF wa_zlest0108-fat_contingencia_ecc EQ abap_true.

      IF wa_zlest0108-data_vf_frete IS NOT INITIAL.
        lva_data_mov_custo = wa_zlest0108-data_vf_frete.
      ELSEIF wa_zlest0108-data_custo IS NOT INITIAL..
        lva_data_mov_custo = wa_zlest0108-data_custo.
      ENDIF.

    ENDIF.

    PERFORM memorizar_dt_movimento_badi USING lva_data_mov_custo. "WA_SAIDA-DT_MOVIMENTO

    DATA: i_tknum	TYPE tknum.

    i_tknum = wa_saida_aviso-transp.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_tknum
      IMPORTING
        output = i_tknum.

    TRY .
        zcl_faturamento=>zif_faturamento~get_instance(
          )->get_processo_emissao_docs(
            EXPORTING
              i_tknum       = i_tknum
            IMPORTING
              e_doc_custo    = DATA(e_doc_custo)
              e_conhecimento = DATA(e_conhecimento)
              e_tipo_veiculo = DATA(e_tipo_veiculo)
              e_tp_frete     = DATA(e_tp_frete)
              e_manifesto    = DATA(e_manifesto)
          ).

      CATCH zcx_faturamento INTO DATA(ex_faturamento).
        MESSAGE ex_faturamento->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      CATCH zcx_error INTO DATA(ex_error).
        MESSAGE ex_error->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.

    IF e_tipo_veiculo EQ zif_faturamento=>st_tp_prop_veiculo_proprio.
      DATA(ckfprop) = abap_true.
    ELSE.
      ckfprop = abap_false.
    ENDIF.

    TRY .
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = wa_saida_aviso-agente_frete
          )->ck_parceiro_local_negocio(
          ).

        DATA(rb_cus) = abap_false.

      CATCH zcx_parceiros.    " .
        IF ckfprop EQ abap_true.
          rb_cus = abap_true.
        ELSEIF ckfprop EQ abap_false AND e_tp_frete EQ zif_carga=>st_tp_frete_cpt.
          rb_cus = abap_true.
        ENDIF.
    ENDTRY.

    "Gerar custo
    "IF WA_SAIDA_AVISO-INCO1 = 'CPT'.
    SUBMIT zlesr0013 WITH so_tknum = wa_saida_aviso-transp
                     WITH p_vbeln  = wa_saida_aviso-vbeln
                     WITH p_ebeln  = wa_saida_aviso-ebeln
                     WITH cksetap  = abap_true
                     WITH rb_in    = e_doc_custo    "Documento de custo de frete
                     WITH rb_out   = e_conhecimento "Ordem / Fatura Serviço
                     WITH ckfprop  = ckfprop
                     WITH rb_cus   = rb_cus
                     WITH rb_dtfat = lva_data_mov_custo AND RETURN.

    "ELSE.
    "
    "  SUBMIT ZLESR0013 WITH SO_TKNUM = WA_SAIDA_AVISO-TRANSP
    "                   WITH P_VBELN  = WA_SAIDA_AVISO-VBELN
    "                   WITH P_EBELN  = WA_SAIDA_AVISO-EBELN
    "                   WITH RB_DTFAT = SY-DATUM "WA_SAIDA-DT_MOVIMENTO
    "                   WITH RB_OUT   = ''
    "                   WITH RB_CUS   = 'X'
    "  AND RETURN.
    "ENDIF.

    CLEAR: vl_fknum,vl_ov_frete,vl_fatura_frete.
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_fknum.
    GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_ov_frete.
    GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fatura_frete.

    IF vl_fknum IS NOT INITIAL.
      wa_saida_aviso-doccus  = vl_fknum.
      "
      IF e_conhecimento EQ abap_true.
        wa_saida_aviso-ovserv  = vl_ov_frete.
        wa_saida_aviso-fatserv = vl_fatura_frete.
        "
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_saida_aviso-fatserv
          IMPORTING
            output = wa_saida_aviso-fatserv.
      ENDIF.
      MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING doccus ovserv fatserv.


      IF vl_fatura_frete IS NOT INITIAL.
        UPDATE zlest0108 SET st_proc      = '07' " Fatura Frete
                             fknum        = vl_fknum
                             ov_frete     = vl_ov_frete
                             fatura_frete = vl_fatura_frete
          WHERE ebeln = wa_saida_aviso-ebeln
            AND vbeln = wa_saida_aviso-vbeln.

        wa_saida_aviso-st_proc = '07'.

        MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING st_proc.

      ELSEIF vl_ov_frete IS NOT INITIAL.
        UPDATE zlest0108 SET st_proc     = '06' " OV_FRETE
                             fknum        = vl_fknum
                             ov_frete     = vl_ov_frete
                             fatura_frete = vl_fatura_frete
         WHERE ebeln = wa_saida_aviso-ebeln
           AND vbeln = wa_saida_aviso-vbeln.
        wa_saida_aviso-st_proc = '06'.

        MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING st_proc.

      ELSEIF vl_fknum IS NOT INITIAL.
        UPDATE zlest0108 SET st_proc      = '05' " Doc.Custo
                             fknum        = vl_fknum
                             ov_frete     = vl_ov_frete
                             fatura_frete = vl_fatura_frete
         WHERE ebeln = wa_saida_aviso-ebeln
           AND vbeln = wa_saida_aviso-vbeln.

        wa_saida_aviso-st_proc = '05'.

        MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING st_proc.

        IF e_conhecimento EQ abap_false. " Finaliza processo com a Fatura serviço gerada

          "Verificar se Nota Eletrônica """"""""""""""""""""""""""""""""""""""""""""""""""
          IF e_manifesto EQ abap_true.
            SELECT * INTO TABLE @DATA(it_zlest0110)
              FROM zlest0110
             WHERE vbeln EQ @wa_saida_aviso-vbeln
               AND chave NE @space.

            IF sy-subrc IS NOT INITIAL.
              e_manifesto = abap_false.
            ENDIF.
          ENDIF.
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          CASE e_manifesto.
            WHEN abap_false.

              UPDATE zlest0108 SET st_proc     = '99' " Finalizado
               WHERE ebeln = wa_saida_aviso-ebeln
                 AND vbeln = wa_saida_aviso-vbeln.

              CLEAR wa_saida_aviso-icon.

              "Apaga log Erros
              " DELETE FROM ZLEST0100 WHERE EBELN = WA_SAIDA_AVISO-EBELN
              "                         AND VBELN = WA_SAIDA_AVISO-VBELN.

              wa_saida_aviso-dacte    = icon_icon_list.
              wa_saida_aviso-st_proc = '99'.

              MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING  dacte icon st_proc.

            WHEN abap_true.
              wa_saida_aviso-damdfe   = icon_execute_object.
              MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING  damdfe.

          ENDCASE.

          CALL METHOD wa_alv_aviso_rec->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.

      ENDIF.
      "refresh na tela
      CALL METHOD wa_alv_aviso_rec->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDIF.

  ELSE.
    MESSAGE 'Erro ao gerar transporte!' TYPE 'I'.
  ENDIF.

*      ELSEIF WA_SAIDA-TRANSP NE ICON_ICON_LIST AND WA_SAIDA-TRANSP+0(4) NE '@11@'.
*        SET PARAMETER ID 'TNR' FIELD WA_SAIDA-TRANSP+0(10).
*        CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
*
*      ENDIF.
ENDFORM.                    " GERAR_DOC_TRANSP

FORM f_gerar_vt USING    p_saida_aviso  TYPE ty_saida_aviso
                         p_wa_zlest0108 TYPE zlest0108
                CHANGING p_erro.

  TYPES: BEGIN OF ty_vbpa,
           parvw TYPE vbpa-parvw,
           kunnr TYPE vbpa-kunnr,
           lifnr TYPE vbpa-lifnr,
         END OF ty_vbpa.

  DATA: wl_tvro    TYPE tvro,
        it_vbpa    TYPE TABLE OF ty_vbpa,
        wl_vbpa    TYPE ty_vbpa,
        wl_a917    TYPE a917,

        v_auart    TYPE vbak-auart,
        v_bsart    TYPE ekko-bsart,
        v_lifnr    TYPE lfa1-lifnr,
        v_ktokk    TYPE lfa1-ktokk,
        v_lzonel   TYPE lfa1-lzone,
        v_lzonek   TYPE kna1-lzone,
        v_kunnr    TYPE vbpa-kunnr,
        v_stcd1    TYPE kna1-stcd1,
        v_route    TYPE trolz-route,
        v_knote    TYPE tvkn-knote,
        v_msgi(50).

  DATA: ws_zlest0135 TYPE zlest0135,
        e_msg_erro   TYPE bapiret2,
        e_status     TYPE sy-subrc.



  " Tabelas BAPI
  DATA: st_headerdata       TYPE bapishipmentheader,
        st_stagedata        TYPE bapishipmentstage,
        t_stagedata         TYPE TABLE OF bapishipmentstage,
        t_itemdata          TYPE TABLE OF bapishipmentitem,
        st_itemdata         TYPE bapishipmentitem,

        st_headerdata2      TYPE bapishipmentheader,
        st_headerdataaction TYPE bapishipmentheaderaction,
        lc_saida            TYPE ty_saida_aviso.


  IF p_saida_aviso-inco1 NE 'FOB' AND p_saida_aviso-inco1 NE 'CFR'.
    IF p_saida_aviso-kbetr LE 0.
      MESSAGE i000(z01) WITH 'Não existe valor de frete cadastrado.'
                             'Solicite à transportadora da sua região'.
      EXIT.
    ENDIF.

    IF p_saida_aviso-cont_fre GT 1.
      MESSAGE i000(z01) WITH 'Existe mais de um valor de frete cadastrado.'
                             'Solicite a regularização à transportadora '
                             'da sua região'.
      EXIT.
    ENDIF.
  ENDIF.

  IF r_atrib IS INITIAL.

    IF p_saida_aviso-inco1 = 'CIF'.

******"Ajuste na consulta situação do transportador / USER HISTORY "66690 / ABAP AOENNING - 21/06/2023.
*      SELECT SINGLE *
*       FROM tvarvc INTO @DATA(lwa_tvarv_zseg)
*      WHERE name = 'ZLES0136_EXC_ZSEG'
*        AND low  = @p_wa_zlest0108-agente_frete.

      CLEAR: ws_zlest0135, e_status.
      CALL FUNCTION 'Z_LES_EXC_ZSEG'
        EXPORTING
          i_placa       = p_wa_zlest0108-placa_cav
          i_ck_consulta = abap_true
        IMPORTING
          e_msg_erro    = e_msg_erro
          e_status      = e_status.


*** "Ajuste na consulta situação do transportador / USER HISTORY "66690 / AOENNING.

      CASE e_status.
        WHEN 1. "Se houve erro na comunicação da API.
          MESSAGE i000(z01) WITH e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3.
          EXIT.

        WHEN 2. "Check condição tp_transportador "ETC Não equiparado.

          "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
          SELECT SINGLE *
            FROM a917
            INTO wl_a917
           WHERE kappl = 'F'
             AND kschl        = 'ZSEG'
             AND matnr        = p_saida_aviso-matnr
             AND tdlnr        = p_wa_zlest0108-agente_frete
             AND kfrst        = ''
             AND datbi        GE sy-datum.

          IF sy-subrc NE 0.
            CONCATENATE 'Agente:' p_wa_zlest0108-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
            MESSAGE i000(z01) WITH 'Não existe % p/ desc. de Seguro. Mat.:'
                    p_saida_aviso-matnr
                    v_msgi.
            EXIT.
          ENDIF.

          "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
          SELECT SINGLE *
            FROM a917
            INTO wl_a917
           WHERE kappl = 'F'
             AND kschl = 'ZIOF'
             AND matnr = p_saida_aviso-matnr
             AND tdlnr = p_wa_zlest0108-agente_frete
             AND kfrst = ''
             AND datbi GE sy-datum.

          IF sy-subrc NE 0.
            CONCATENATE 'Agente:' p_wa_zlest0108-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
            MESSAGE i000(z01) WITH 'Não existe % p/ desc. de IOF Mat.:'
                    p_saida_aviso-matnr
                    v_msgi.
            EXIT.
          ENDIF.
        WHEN 3. "Se não localizou dados na API.

          MESSAGE i000(z01) WITH e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3.
          EXIT.

        WHEN OTHERS.
      ENDCASE.
    ENDIF.

  ELSE.

    SELECT SINGLE lifnr, ktokk
    FROM lfa1
    INTO @DATA(wlfa1_aux)
    WHERE lifnr = @p_wa_zlest0108-agente_frete.

    IF wlfa1_aux-ktokk = 'ZFIC'.

******"Ajuste na consulta situação do transportador / USER HISTORY "66690 / ABAP AOENNING - 21/06/2023.
*      SELECT SINGLE *
*        FROM tvarvc INTO lwa_tvarv_zseg
*       WHERE name = 'ZLES0136_EXC_ZSEG'
*         AND low  = p_wa_zlest0108-agente_frete.

      CLEAR: e_status, e_msg_erro.
      CALL FUNCTION 'Z_LES_EXC_ZSEG'
        EXPORTING
          i_placa       = p_wa_zlest0108-placa_cav
          i_ck_consulta = abap_true
        IMPORTING
          e_status      = e_status
          e_msg_erro    = e_msg_erro.


*** "Ajuste na consulta situação do transportador / USER HISTORY "66690 / AOENNING.

      CASE e_status.
        WHEN 1. "Se houve erro na comunicação da API.

          MESSAGE i000(z01) WITH e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3.
          EXIT.

        WHEN 2. "Check condição tp_transportador "ETC Não equiparado.
          IF p_saida_aviso-inco1 = 'CIF'.
            "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
            SELECT SINGLE *
              FROM a917
              INTO wl_a917
             WHERE kappl = 'F'
               AND kschl        = 'ZSEG'
               AND matnr        = p_saida_aviso-matnr
               AND tdlnr        = p_wa_zlest0108-agente_frete
               AND kfrst        = ''
               AND datbi        GE sy-datum.

            IF sy-subrc NE 0.
              CONCATENATE 'Agente:' p_wa_zlest0108-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
              MESSAGE i000(z01) WITH 'Não existe % p/ desc. de Seguro. Mat.:'
                      p_saida_aviso-matnr
                      v_msgi.
              EXIT.
            ENDIF.

            "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
            SELECT SINGLE *
              FROM a917
              INTO wl_a917
             WHERE kappl = 'F'
               AND kschl = 'ZIOF'
               AND matnr = p_saida_aviso-matnr
               AND tdlnr = p_wa_zlest0108-agente_frete
               AND kfrst = ''
               AND datbi GE sy-datum.

            IF sy-subrc NE 0.
              CONCATENATE 'Agente:' p_wa_zlest0108-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
              MESSAGE i000(z01) WITH 'Não existe % p/ desc. de IOF Mat.:'
                      p_saida_aviso-matnr
                      v_msgi.
              EXIT.
            ENDIF.
          ENDIF.
        WHEN 3. "Devolver erro se não localizou dados na API.

          MESSAGE i000(z01) WITH e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3.
          EXIT.

        WHEN OTHERS.
      ENDCASE.

    ENDIF.

  ENDIF.

  " preenche BAPI
  " Cabeçalho
  CLEAR st_headerdata.

  st_headerdata-service_agent_id        = p_wa_zlest0108-agente_frete.

  IF p_wa_zlest0108-id_ordem IS NOT INITIAL.

    st_headerdata-zid_ordem  = p_wa_zlest0108-id_ordem.

    SELECT SINGLE viagem_id INTO @DATA(lv_viagem_id) FROM zlest0185
      WHERE id_ordem  =  @p_wa_zlest0108-id_ordem.

    IF sy-subrc EQ 0.
      st_headerdata-zid_viagem = lv_viagem_id.
    ENDIF.

  ENDIF.

  "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>
  IF st_headerdata-zid_viagem IS INITIAL AND p_wa_zlest0108-viagem_id IS NOT INITIAL.
    st_headerdata-zid_viagem = p_wa_zlest0108-viagem_id.
  ENDIF.
  "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP <<---


  st_headerdata-service_level           = '1'.
  st_headerdata-shipping_type           = '01'.
  st_headerdata-status_plan             = 'X'.
  st_headerdata-status_checkin          = 'X'.
  st_headerdata-status_load_start       = 'X'.
  st_headerdata-special_procedure_id 	  = '0001'.
  st_headerdata-shpmnt_cost_rel         = 'X'.
  st_headerdata-shipment_type           = p_saida_aviso-shtyp.
  st_headerdata-trans_plan_pt           = p_saida_aviso-branch.

  v_lifnr = p_wa_zlest0108-cod_loc_coleta.

  SELECT SINGLE lzone ktokk
    FROM lfa1
    INTO (v_lzonel, v_ktokk)
    WHERE lifnr   = v_lifnr.

  CHECK sy-subrc = 0.

  CLEAR v_route.

  v_route = p_saida_aviso-route.
  st_headerdata-shipment_route  = v_route.

  SELECT SINGLE *
    FROM tvro
    INTO wl_tvro
   WHERE route    = v_route.

  CHECK sy-subrc = 0.
  IF wl_tvro-traztd GT 24.
    wl_tvro-traztd = wl_tvro-traztd / 24.
  ENDIF.

  st_headerdata-distance        = wl_tvro-distz.
  st_headerdata-distance_unit   = wl_tvro-medst.
  st_headerdata-time_travel     =	wl_tvro-traztd.
  st_headerdata-time_unit       =	'H'.

*ETAPA
  CLEAR st_stagedata.
  REFRESH t_stagedata.
  st_stagedata-stage_cat    = '1'.
  st_stagedata-stage_seq 	= '0001'.
  st_stagedata-shipping_type  =  '01'.
  st_stagedata-service_agent  = p_wa_zlest0108-agente_frete.

  "Local de Partida
  IF v_ktokk = 'ZFIC'.
    st_stagedata-org_shipp_dpmnt = v_lifnr+6(4).
  ELSE.
    st_stagedata-org_suppl  = v_lifnr.
  ENDIF.

  "Local de entrega/chegada
*  SELECT SINGLE KUNNR
*    FROM VBPA
*    INTO V_KUNNR
*   WHERE VBELN   = WA_ZSDT0001-DOC_REM
*     AND PARVW   = 'LR'.  "Local de entrega

  "Local de entrega/chegada
  v_kunnr = p_wa_zlest0108-cod_loc_entrega.

  CHECK v_kunnr IS NOT INITIAL.

  SELECT SINGLE shtyp laufk
    FROM tvtk
    INTO wa_tvtk
   WHERE shtyp = p_saida_aviso-shtyp.

  st_stagedata-leg_indicator = wa_tvtk-laufk. "Código de Percurso

  "Se Código de Percurso  igual a 1:  Percurso preliminar
  IF wa_tvtk-laufk = 1.
    CLEAR v_knote.

    SELECT SINGLE knote
      FROM tvkn
      INTO v_knote
     WHERE kunnr   = v_kunnr.

    IF sy-subrc = 0.
      st_stagedata-dest_point   = v_knote.
    ELSE.
      st_stagedata-dest_point   = v_kunnr.
    ENDIF.

    "Se Código de Percurso  igual a 4:  Percurso direto
  ELSEIF wa_tvtk-laufk = 4.
    IF st_headerdata-shipment_type  =  'Z004'.
      SELECT SINGLE stcd1
        FROM kna1
        INTO v_stcd1
       WHERE kunnr = v_kunnr.

      SELECT SINGLE lifnr
        FROM lfa1
        INTO v_lifnr
       WHERE stcd1 = v_stcd1.

      st_stagedata-dest_suppl  = v_lifnr.

    ELSE.
      st_stagedata-dest_cust    = v_kunnr. "Local de entrega (V_KUNNR)
    ENDIF.

  ENDIF.

*  ST_STAGEDATA-DEST_PLANT    =     xxxxx     (Local de chegada: centro)

  APPEND st_stagedata TO t_stagedata.

* Dados itens
  CLEAR st_itemdata.
  REFRESH t_itemdata.
  st_itemdata-delivery      =  p_saida_aviso-vbeln.  "WA_ZSDT0001-DOC_REM.
  st_itemdata-itenerary     =   '000010'.
  APPEND st_itemdata TO t_itemdata.

  CLEAR v_tknum.
  REFRESH t_return_vt.
  "------>Gera o Transporte <------
  CALL FUNCTION 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      headerdata = st_headerdata
    IMPORTING
      transport  = v_tknum
    TABLES
      itemdata   = t_itemdata
      stagedata  = t_stagedata
      return     = t_return_vt.

  IF v_tknum IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    p_erro = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    CLEAR : st_headerdataaction, st_headerdata2.
    REFRESH: t_return_vt.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_tknum
      IMPORTING
        output = st_headerdata2-shipment_num.

    st_headerdata2-status_load_end       = 'X'.
    st_headerdataaction-status_load_end  = 'C'.

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = st_headerdata2
        headerdataaction = st_headerdataaction
      TABLES
        return           = t_return_vt.
*
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    IF p_saida_aviso-inco1 EQ 'CIF'.   "<<<------"188635 - NMS ------->>>
      CALL FUNCTION 'Z_LES_VERIFICA_PED_ADM'
        EXPORTING
          p_tknum      = st_headerdata2-shipment_num
        EXCEPTIONS
          adiantamento = 1
          pedagio      = 2
          OTHERS       = 3.
    ENDIF.                             "<<<------"188635 - NMS ------->>>

    IF sy-subrc IS NOT INITIAL.
      p_erro = abap_true.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      "Estorna VT
      REFRESH t_itemdata.
      CLEAR st_itemdata.
      st_itemdata-delivery  = p_saida_aviso-vbeln.
      st_itemdata-itenerary = '0010'.
      APPEND st_itemdata TO t_itemdata.
      MOVE-CORRESPONDING p_saida_aviso TO lc_saida.
      lc_saida-transp = st_headerdata2-shipment_num.
      PERFORM elimina_vt TABLES t_itemdata CHANGING lc_saida.
    ELSE.

      st_headerdata2-status_compl             = 'X'.
      st_headerdata2-status_shpmnt_start      = 'X'.
      st_headerdataaction-status_compl        = 'C'.
      st_headerdataaction-status_shpmnt_start = 'C'.

      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
        EXPORTING
          headerdata       = st_headerdata2
          headerdataaction = st_headerdataaction
        TABLES
          return           = t_return_vt.
*
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      "atualiza valor do frete Historico
      UPDATE zlest0108 SET kbetr = p_saida_aviso-kbetr  WHERE ebeln = p_wa_zlest0108-ebeln
                                                          AND vbeln = p_wa_zlest0108-vbeln.

      p_erro = 'N'.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GERAR_VT


*&---------------------------------------------------------------------*
*&      Form  f_atual_frete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_atual_frete  USING p_zlest0108 TYPE zlest0108 CHANGING p_wa_saida TYPE ty_saida_aviso.
  DATA:          v_cont_fre          TYPE i.

  DATA: lva_viagem_id TYPE zlest0185-viagem_id.

  v_cont_fre = 0.

  CLEAR: wa_lfa1,wa_kna1,wa_a900,wa_a910,wa_a911,wa_a915,wa_a918,wa_a919, p_wa_saida-kbetr.

  SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = p_zlest0108-cod_loc_coleta.
  SELECT SINGLE * INTO wa_kna1 FROM kna1 WHERE kunnr = p_zlest0108-cod_loc_entrega.

  "Vlr Frete
  SELECT SINGLE * INTO wa_a900 FROM a900 AS a WHERE shtyp = p_wa_saida-shtyp
                                                AND tdlnr = p_wa_saida-agente_frete
                                                AND route = p_wa_saida-route
                                                AND add01 = p_wa_saida-add01
                                                AND datab  LE sy-datum
                                                AND datbi  GE sy-datum
                                                AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
  IF sy-subrc = 0.
    CLEAR: wa_konp.
    SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh = wa_a900-knumh.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO wa_a910 FROM a910 AS a WHERE shtyp  = p_wa_saida-shtyp
                                                AND tdlnr  = p_wa_saida-agente_frete
                                                AND lzonea = wa_lfa1-lzone
                                                AND lzonez = wa_kna1-lzone
                                                AND datab  LE sy-datum
                                                AND datbi  GE sy-datum
                                                AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).

  IF sy-subrc = 0.
    SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh = wa_a910-knumh.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO wa_a911 FROM a911 AS a WHERE shtyp = p_wa_saida-shtyp
                                                AND tdlnr = p_wa_saida-agente_frete
                                                AND route = p_wa_saida-route
                                                AND datab  LE sy-datum
                                                AND datbi  GE sy-datum
                                                AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).

  IF sy-subrc = 0.
    CLEAR: wa_konp.
    SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh = wa_a911-knumh.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO wa_a915 FROM a915 AS a WHERE shtyp  = p_wa_saida-shtyp
                                                AND tdlnr  = p_wa_saida-agente_frete
                                                AND lzonea = wa_lfa1-lzone
                                                AND lzonez = wa_kna1-lzone
                                                AND add01  = p_wa_saida-add01
                                                AND datab  LE sy-datum
                                                AND datbi  GE sy-datum
                                                AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
  IF sy-subrc = 0.
    CLEAR: wa_konp.
    SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh = wa_a915-knumh.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO wa_a918 FROM a918 AS a WHERE shtyp  = p_wa_saida-shtyp
                                                AND tdlnr  = p_wa_saida-agente_frete
                                                AND matnr  = p_wa_saida-matnr
                                                AND lzonea = wa_lfa1-lzone
                                                AND lzonez = wa_kna1-lzone
                                                AND add01  = p_wa_saida-add01
                                                AND datab  LE sy-datum
                                                AND datbi  GE sy-datum
                                                AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
  IF sy-subrc = 0.
    CLEAR: wa_konp.
    SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh = wa_a918-knumh.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO wa_a919 FROM a919 AS a WHERE shtyp  = p_wa_saida-shtyp
                                                AND tdlnr  = p_wa_saida-agente_frete
                                                AND matnr  = p_wa_saida-matnr
                                                AND lzonea = wa_lfa1-lzone
                                                AND lzonez = wa_kna1-lzone
                                                AND datab  LE sy-datum
                                                AND datbi  GE sy-datum
                                                AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
  IF sy-subrc = 0.
    CLEAR: wa_konp.
    SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh = wa_a919-knumh.
    IF sy-subrc = 0.
      p_wa_saida-kbetr = wa_konp-kbetr.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDIF.

  CLEAR: lva_viagem_id.
  IF p_zlest0108-id_ordem IS NOT INITIAL.
    SELECT SINGLE viagem_id INTO lva_viagem_id FROM zlest0185
     WHERE id_ordem EQ p_zlest0108-id_ordem.

    IF sy-subrc NE 0.
      CLEAR: lva_viagem_id.
    ENDIF.
  ENDIF.

  "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>
  IF lva_viagem_id IS INITIAL AND p_zlest0108-viagem_id IS NOT INITIAL.
    lva_viagem_id = p_zlest0108-viagem_id.
  ENDIF.
  "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP <<---

*--------------------------------------
*-- Tipo do Contrato
*--------------------------------------
*  DATA lv_add01 TYPE zde_info_frete-add01.
*  TRY.
*      zcl_veiculos=>zif_veiculos~get_instance(
*         )->set_veiculo(
*            EXPORTING
*              i_placa         = p_zlest0108-placa_cav
*         )->get_tipo_contrato(
*            IMPORTING
*              e_tipo_contrato = lv_add01
*         ).
*
*    CATCH zcx_veiculos INTO DATA(ex_veiculo).    "
*      ex_veiculo->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
*      RAISE EXCEPTION TYPE zcx_integracao
*        EXPORTING
*          textid = VALUE #( msgid  = zcx_integracao=>zcx_erro_geral-msgid
*                            msgno  = zcx_integracao=>zcx_erro_geral-msgno
*                            attr1  = CONV #( sy-msgv1 ) attr2  = CONV #( sy-msgv2 ) attr3  = CONV #( sy-msgv3 ) attr4  = CONV #( sy-msgv4 ) )
*          msgty  = 'E'
*          msgid  = zcx_integracao=>zcx_erro_geral-msgid
*          msgno  = zcx_integracao=>zcx_erro_geral-msgno
*          msgv1  = sy-msgv1
*          msgv2  = sy-msgv2
*          msgv3  = sy-msgv3
*          msgv4  = sy-msgv4.
*  ENDTRY.
*
*  CONDENSE lv_add01 NO-GAPS.

  IF lva_viagem_id IS NOT INITIAL.
    SELECT SINGLE * INTO wa_a942 FROM a942 AS a
    WHERE shtyp  = p_wa_saida-shtyp
            AND sdabw = '0001'
            AND id_viagem = lva_viagem_id
            AND datab  LE sy-datum
            AND datbi  GE sy-datum
            AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh =
                                 a~knumh AND b~loevm_ko EQ '' ).
    IF sy-subrc = 0.
      CLEAR: wa_konp.
      SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh = wa_a942-knumh.
      IF sy-subrc = 0.
        p_wa_saida-kbetr = wa_konp-kbetr.
        ADD 1 TO v_cont_fre.
      ENDIF.
    ENDIF.
  ENDIF.

  p_wa_saida-cont_fre  = v_cont_fre.

ENDFORM.                    "f_atual_frete



*&---------------------------------------------------------------------*
*&      Form  ELIMINA_VT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ITEMDATA  text
*      -->P_V_TKNUM  text
*----------------------------------------------------------------------*
FORM elimina_vt  TABLES   tl_itemdata
                 CHANGING p_wa_saida TYPE ty_saida_aviso.

  DATA: st_headerdata2      TYPE bapishipmentheader,
        st_headerdataaction TYPE bapishipmentheaderaction,
        t_itemdataaction    TYPE TABLE OF bapishipmentitemaction WITH HEADER LINE,
        st_headerdata       TYPE bapishipmentheader.


  CLEAR : st_headerdataaction, st_headerdata, t_itemdataaction, st_headerdata2.
  REFRESH: t_itemdataaction,t_return_vt.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_wa_saida-transp+0(10)
    IMPORTING
      output = st_headerdata2-shipment_num.

  st_headerdataaction-shipment_num = 'D'.
  st_headerdataaction-service_agent_id = 'D'.

  LOOP AT tl_itemdata INTO st_itemdata.
    MOVE: 'D' TO t_itemdataaction-delivery,
          'D' TO t_itemdataaction-itenerary.

    APPEND t_itemdataaction.
    CLEAR: t_itemdataaction.

  ENDLOOP.
*
  REFRESH t_return_vt.
  CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
    EXPORTING
      headerdata       = st_headerdata2
      headerdataaction = st_headerdataaction
    TABLES
      itemdata         = tl_itemdata
      itemdataaction   = t_itemdataaction
      return           = t_return_vt.

  READ TABLE t_return_vt WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    "atualiza valor do frete Historico
    UPDATE zlest0108 SET kbetr = 0  WHERE vbeln = p_wa_saida-vbeln
                                      AND ebeln = p_wa_saida-ebeln.


    p_wa_saida-st_proc         = ''.
    p_wa_saida-transp          = icon_execute_object.
    UPDATE zlest0108 SET st_proc      = ''
                         doc_transp   = ''
       WHERE vbeln = p_wa_saida-vbeln
         AND ebeln = p_wa_saida-ebeln.
  ELSE.
    REFRESH it_zlest0111.
    CLEAR vl_ponteiro.
    SELECT  MAX( cont )
       FROM zlest0111
       INTO vl_ponteiro
      WHERE vbeln = p_wa_saida-vbeln
        AND ebeln = p_wa_saida-ebeln.

    IF sy-subrc = 0.
      ADD 1 TO vl_ponteiro.
    ELSE.
      vl_ponteiro = 1.
    ENDIF.
    LOOP AT t_return_vt.
      wa_zlest0111-vbeln    = p_wa_saida-vbeln.
      wa_zlest0111-ebeln    = p_wa_saida-ebeln.
      wa_zlest0111-msgtyp   = 'E'.
      wa_zlest0111-msgspra  = sy-langu.
      wa_zlest0111-msgid    = 'LES'.
      wa_zlest0111-msgnr    = '000'.
      wa_zlest0111-msgv1    = t_return_vt-message.
      wa_zlest0111-data     = sy-datum.
      wa_zlest0111-hora     = sy-uzeit.
      wa_zlest0111-usuario  = sy-uname.
      wa_zlest0111-cont     = vl_ponteiro.

      APPEND wa_zlest0111 TO it_zlest0111.
      ADD 1 TO vl_ponteiro.
    ENDLOOP.
    MODIFY zlest0111 FROM TABLE it_zlest0111.

  ENDIF.
ENDFORM.                    " ELIMINA_VT

FORM memorizar_dt_movimento_badi  USING p_data_rem TYPE ledat.

  TYPES:
    BEGIN OF tab_type,
      para TYPE string,
      dobj TYPE string,
    END OF tab_type.

  DATA: line TYPE tab_type,
        itab TYPE STANDARD TABLE OF tab_type,
        id   TYPE c LENGTH 10 VALUE 'ROMRETRO'.

  line-para = 'P1'.
  line-dobj = 'P_DATA_REM'.
  APPEND line TO itab.

  EXPORT (itab) TO MEMORY ID 'ROMRETRO'.

ENDFORM.                    " MEMORIZAR_DT_MOVIMENTO_BADI


FORM montar_layout_log.
  REFRESH: estrutura.

  PERFORM estrutura_alv_log USING:
     1  ''   ''            'TI_ZLEST0100' 'CONT'     'Seq'         ' ',
     1  ''   ''            'TI_ZLEST0100' 'MSGID'    'ID'          ' ',
     1  ''   ''            'TI_ZLEST0100' 'MSGV1'    'Menssagem'   '60',
     1  ''   ''            'TI_ZLEST0100' 'DATA'     'Data'        '10',
     1  ''   ''            'TI_ZLEST0100' 'HORA'     'Hora'        '10',
     1  ''   ''            'TI_ZLEST0100' 'USUARIO'  'Usuário'     '15'.


ENDFORM.                    " MONTAR_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorno_cte CHANGING pa_saida TYPE ty_saida_aviso.

  "Cancela fatura
  REFRESH: t_success, t_return_vt.
  IF '99_07' CS pa_saida-st_proc .
    SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
         FROM j_1bnflin
         INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
         INTO (vl_bukrs,vl_docnum)
         WHERE j_1bnflin~refkey = pa_saida-fatserv.

    SELECT SINGLE * FROM zcte_ciot INTO wa_zcte_ciot WHERE docnum EQ vl_docnum.
    IF ( sy-subrc EQ 0 ).
      IF ( ( wa_zcte_ciot-st_ciot NE 8 ) AND (  wa_zcte_ciot-st_ciot NE 0 ) OR ( wa_zcte_ciot-st_ciot NE 0 ) AND ( wa_zcte_ciot-st_ciot NE 8 ) ).
        MESSAGE e000(z01) WITH 'Necessário cancelar a viagem'.
        EXIT.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1' "Cancel Customer Individual Billing Document
      EXPORTING
        billingdocument = pa_saida-fatserv           " fatura_frete.
      TABLES
        return          = t_return         " bapireturn1   Table of Error Messages Entered
        success         = t_success.       " bapivbrksuccess  Table of Successfully Processed Documents
  ENDIF.

  IF t_success[] IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    pa_saida-fatserv         = icon_icon_list.
    UPDATE zlest0108 SET st_proc      = '06'                   "06
                         fatura_frete = ''
    WHERE vbeln = pa_saida-vbeln
      AND ebeln = pa_saida-ebeln.

    WAIT UP TO 5 SECONDS.
  ELSE.
    READ TABLE t_return_vt WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      REFRESH it_zlest0111.
      CLEAR vl_ponteiro.
      SELECT  MAX( cont )
       FROM zlest0111
        INTO vl_ponteiro
       WHERE vbeln = pa_saida-vbeln
         AND ebeln = pa_saida-ebeln.

      IF sy-subrc = 0.
        ADD 1 TO vl_ponteiro.
      ELSE.
        vl_ponteiro = 1.
      ENDIF.
      LOOP AT t_return_vt.
        wa_zlest0111-mandt   = sy-mandt.
        wa_zlest0111-vbeln   = pa_saida-vbeln.
        wa_zlest0111-ebeln   = pa_saida-ebeln.
        wa_zlest0111-msgtyp  = 'E'.
        wa_zlest0111-msgspra = sy-langu.
        wa_zlest0111-msgid   = 'LES'.
        wa_zlest0111-msgnr   = '000'.
        wa_zlest0111-msgv1   = t_return_vt-message.
        wa_zlest0111-data    = sy-datum.
        wa_zlest0111-hora    = sy-uzeit.
        wa_zlest0111-usuario = sy-uname.
        wa_zlest0111-cont    = vl_ponteiro.

        APPEND wa_zlest0111 TO it_zlest0111.
        ADD 1 TO vl_ponteiro.
      ENDLOOP.
      MODIFY zlest0111 FROM TABLE it_zlest0111.
      EXIT.
    ENDIF.
  ENDIF.

  REFRESH t_return_vt.
  IF ( '99_07_06' CS pa_saida-st_proc ) AND
     ( pa_saida-ovserv NE icon_icon_list ) AND
     ( pa_saida-ovserv IS NOT INITIAL ).

    "Bloqueia Ordem do frete
    CLEAR: wl_orderheaderin,wl_orderheaderinx.
    REFRESH:tl_bapiparex.
    wl_bape_vbak-vbeln   = pa_saida-ovserv.
    wl_bape_vbak-tknum   = ''.
    sl_bapiparex-structure     = 'BAPE_VBAK'.
    sl_bapiparex-valuepart1    = wl_bape_vbak.
    APPEND sl_bapiparex TO tl_bapiparex.
    CLEAR sl_bapiparex.
    wl_bape_vbakx-vbeln  = pa_saida-ovserv.
    wl_bape_vbakx-tknum  = 'X'.
    sl_bapiparex-structure     = 'BAPE_VBAKX'.
    sl_bapiparex-valuepart1 = wl_bape_vbakx.
    APPEND sl_bapiparex TO tl_bapiparex.

    wl_orderheaderin-bill_block  = '10'.
    wl_orderheaderinx-updateflag = 'U'.
    wl_orderheaderinx-bill_block = 'X'.
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        salesdocument    = pa_saida-ovserv " ov_frete
        order_header_in  = wl_orderheaderin
        order_header_inx = wl_orderheaderinx
      TABLES
        return           = t_return_vt
        extensionin      = tl_bapiparex.
    "
    READ TABLE t_return_vt WITH KEY type = 'E'.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.

      pa_saida-ovserv          = icon_icon_list.
      UPDATE zlest0108 SET st_proc    = '05'                     "05
                           ov_frete   = ''
       WHERE vbeln = pa_saida-vbeln
         AND ebeln = pa_saida-ebeln.
    ELSE.
      READ TABLE t_return_vt WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        REFRESH it_zlest0111.
        CLEAR vl_ponteiro.
        SELECT  MAX( cont )
           FROM zlest0111
           INTO vl_ponteiro
          WHERE vbeln = pa_saida-vbeln
            AND ebeln = pa_saida-ebeln.

        IF sy-subrc = 0.
          ADD 1 TO vl_ponteiro.
        ELSE.
          vl_ponteiro = 1.
        ENDIF.

        LOOP AT t_return_vt.
          wa_zlest0111-mandt      = sy-mandt.
          wa_zlest0111-vbeln = pa_saida-vbeln.
          wa_zlest0111-ebeln = pa_saida-ebeln.
          wa_zlest0111-msgtyp     = 'E'.
          wa_zlest0111-msgspra    = sy-langu.
          wa_zlest0111-msgid      = 'LES'.
          wa_zlest0111-msgnr      = '000'.
          wa_zlest0111-msgv1      = t_return_vt-message.
          wa_zlest0111-data       = sy-datum.
          wa_zlest0111-hora       = sy-uzeit.
          wa_zlest0111-usuario    = sy-uname.
          wa_zlest0111-cont       = vl_ponteiro.

          APPEND wa_zlest0111 TO it_zlest0111.
          ADD 1 TO vl_ponteiro.
        ENDLOOP.
        MODIFY zlest0111 FROM TABLE it_zlest0111.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF '99_07_06_05_04' CS pa_saida-st_proc.
    PERFORM f_estorno_custo CHANGING pa_saida.
  ENDIF.

ENDFORM.                    " F_ESTORNO_CTE

*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM f_estorno_custo  CHANGING    p_wa_saida TYPE ty_saida_aviso.
  DATA vdata(10).
  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO vdata.
  IF '99_07_06_05' CS p_wa_saida-st_proc.
    "Estornar o documento de custo
    REFRESH ti_bdcdata.
    IF p_wa_saida-inco1 NE 'CPT'.
      "Estornar o documento de custo
      REFRESH ti_bdcdata.
      IF  p_wa_saida-shtyp = 'Z001'.
        PERFORM f_bdc_data USING:
            'SAPMV54A'  '0020'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
            ''          ''      ''   'BDC_OKCODE'       '=UEBP',
            ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(02)',
            ''          ''      ''   'BDC_OKCODE'       '=PLOE',
            ''          ''      ''   'VIM_MARKED(02)'   'X',

            'SAPLSPO1'  '0100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=YES',

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
            ''          ''      ''   'BDC_OKCODE'       '=PDET',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-POSTX',
            ''          ''      ''   'BDC_OKCODE'       '=PABR',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=SICH',
            ''          ''      ''   'VFKPD-SLSTOR'     'X'.
      ELSE.
        PERFORM f_bdc_data USING:
                'SAPMV54A'  '0020'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
                ''          ''      ''   'BDC_OKCODE'       '=UEBP',
                ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

                'SAPMV54A'  '0030'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
                ''          ''      ''   'BDC_OKCODE'       '=PDET',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKP-POSTX',
                ''          ''      ''   'BDC_OKCODE'       '=PABR',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=SICH',
                ''          ''      ''   'VFKPD-SLSTOR'     'X'.
      ENDIF.

    ELSE.
      IF  p_wa_saida-shtyp = 'Z001'.
        PERFORM f_bdc_data USING:
            'SAPMV54A'  '0020'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
            ''          ''      ''   'BDC_OKCODE'       '=UEBP',
            ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(02)',
            ''          ''      ''   'BDC_OKCODE'       '=PLOE',
            ''          ''      ''   'VIM_MARKED(02)'   'X',

            'SAPLSPO1'  '0100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=YES',

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
            ''          ''      ''   'BDC_OKCODE'       '=PDET',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=PABR',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=KLAC',
            ''          ''      ''  'VFKPD-SLSTOR'      'X',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '/00',
            ''          ''      ''   'VFKPD-STDAT'      vdata,

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=SICH'.

      ELSE.
        PERFORM f_bdc_data USING:
                'SAPMV54A'  '0020'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
                ''          ''      ''   'BDC_OKCODE'       '=UEBP',
                ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

                'SAPMV54A'  '0030'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
                ''          ''      ''   'BDC_OKCODE'       '=PDET',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=PABR',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=KLAC',
                ''          ''      ''  'VFKPD-SLSTOR'      'X',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '/00',
                ''          ''      ''   'VFKPD-STDAT'      vdata,

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=SICH'.

      ENDIF.
    ENDIF.

    CLEAR wl_erro.
    PERFORM zf_call_transaction USING 'VI02' CHANGING wl_erro.
    IF wl_erro IS INITIAL.
      COMMIT WORK.
      WAIT UP TO 5 SECONDS.

    ELSE.
      EXIT.
    ENDIF.

    "Eliminar o documento de custo
    REFRESH ti_bdcdata.
    PERFORM f_bdc_data USING:
           'SAPMV54A'  '0020'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
           ''          ''      ''   'BDC_OKCODE'       '=UEBP',
           ''          ''      ''   'VFKK-FKNUM'       p_wa_saida-doccus, "fknum

           'SAPMV54A'  '0030'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'       '/ELOES'.

    CLEAR wl_erro.
    PERFORM zf_call_transaction USING 'VI02' CHANGING wl_erro.
    IF wl_erro IS INITIAL.
      p_wa_saida-doccus          = icon_icon_list.
      p_wa_saida-damdfe          = icon_icon_list.
      UPDATE zlest0108 SET st_proc      = '04'
                           fknum        = ''
       WHERE vbeln = p_wa_saida-vbeln
         AND ebeln = p_wa_saida-ebeln.

      COMMIT WORK.

      WAIT UP TO 2 SECONDS.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.

  IF '99_07_06_05_04' CS p_wa_saida-st_proc.
**** elimina VT
    REFRESH t_itemdata.
    CLEAR st_itemdata.
    st_itemdata-delivery  = p_wa_saida-vbeln.
    st_itemdata-itenerary = '0010'.
    APPEND st_itemdata TO t_itemdata.
    PERFORM elimina_vt TABLES t_itemdata
                       CHANGING p_wa_saida.
  ENDIF.


ENDFORM.                    " F_ESTORNO_CUSTO

FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA


*&---------------------------------------------------------------------*
*&      Form  zf_call_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*      -->P_ERRO     text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  REFRESH it_msg.

  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
        MODE wl_mode
        MESSAGES INTO it_msg.

  READ TABLE it_msg WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0.
    p_erro = 'X'.
    REFRESH it_zlest0111.
    DELETE it_msg WHERE msgtyp NE 'E'.
    CLEAR vl_ponteiro.
    SELECT  MAX( cont )
       FROM zlest0111
       INTO vl_ponteiro
      WHERE vbeln = wa_saida_aviso-vbeln
        AND ebeln = wa_saida_aviso-ebeln.

    IF sy-subrc = 0.
      ADD 1 TO vl_ponteiro.
    ELSE.
      vl_ponteiro = 1.
    ENDIF.
    LOOP AT it_msg INTO wa_msg.

      MESSAGE ID     wa_msg-msgid
              TYPE   wa_msg-msgtyp
              NUMBER wa_msg-msgnr
              WITH   wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4
              INTO   vl_message.

      wa_zlest0111-mandt    = sy-mandt.
      wa_zlest0111-vbeln    = wa_saida_aviso-vbeln.
      wa_zlest0111-ebeln    = wa_saida_aviso-ebeln.
      wa_zlest0111-msgtyp   = wa_msg-msgtyp.
      wa_zlest0111-msgspra  = sy-langu.
      wa_zlest0111-msgid    = wa_msg-msgid.
      wa_zlest0111-msgnr    = wa_msg-msgnr.
      wa_zlest0111-msgv1    = vl_message.
      wa_zlest0111-data     = sy-datum.
      wa_zlest0111-hora     = sy-uzeit.
      wa_zlest0111-usuario  = sy-uname.
      wa_zlest0111-cont     = vl_ponteiro.

      APPEND wa_zlest0111 TO it_zlest0111.
      ADD 1 TO vl_ponteiro.
    ENDLOOP.
    MODIFY zlest0111 FROM TABLE it_zlest0111.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  SOL_ESTORNO_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sol_estorno_cte .

  DATA: var_answer TYPE c.


  CALL METHOD wa_alv_aviso_rec->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK NOT it_sel_rows IS INITIAL.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Confirma o Estorno?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.


  IF sy-tcode NE 'ZLES0113'.
    MESSAGE 'Transação apenas de visualização' TYPE 'I'.
    EXIT.
  ENDIF.

  IF ( lines( it_sel_rows ) NE 1 ).
    MESSAGE 'Selecione uma linha para o estorno' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida_aviso INTO wa_saida_aviso INDEX wa_sel_rows-index.

  IF wa_saida_aviso-st_proc = '99' AND wa_saida_aviso-damdfe(1) NE '@' AND wa_saida_aviso-damdfe IS NOT INITIAL.

    SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
      FROM j_1bnfdoc
     WHERE docnum EQ @wa_saida_aviso-damdfe.

    IF wa_j_1bnfdoc-cancel EQ abap_false.
      MESSAGE 'MDF-e deve ser não autorizada para este processo' TYPE  'I'.
      EXIT.
    ENDIF.

  ENDIF.

  IF '07_06_05_04' CS wa_saida_aviso-st_proc. " Processo estorno não completo

    REFRESH it_zlest0111.

    SELECT SINGLE st_proc
      FROM zlest0108
     INTO  wa_saida_aviso-st_proc
     WHERE vbeln = wa_saida_aviso-vbeln
       AND ebeln = wa_saida_aviso-ebeln.


    PERFORM f_estorno_cte CHANGING wa_saida_aviso.
    IF it_zlest0111[] IS NOT INITIAL.
      wa_saida_aviso-icon = icon_led_red.
    ELSE.
      CLEAR wa_saida_aviso-icon.
    ENDIF.

    MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING   dacte fatserv ovserv transp doccus icon .
    CALL METHOD wa_alv_aviso_rec->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ELSEIF wa_saida_aviso-st_proc = '99'.

    DATA: i_tknum	TYPE tknum.

    i_tknum = wa_saida_aviso-transp.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_tknum
      IMPORTING
        output = i_tknum.

    TRY .
        zcl_faturamento=>zif_faturamento~get_instance(
          )->get_processo_emissao_docs(
            EXPORTING
              i_tknum       = i_tknum
            IMPORTING
              e_doc_custo    = DATA(e_doc_custo)
              e_conhecimento = DATA(e_conhecimento)
              e_tipo_veiculo = DATA(e_tipo_veiculo)
              e_tp_frete     = DATA(e_tp_frete)
          ).

      CATCH zcx_faturamento INTO DATA(ex_faturamento).
        MESSAGE ex_faturamento->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      CATCH zcx_error INTO DATA(ex_error).
        MESSAGE ex_error->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.


    IF e_conhecimento EQ abap_false.

      wa_saida_aviso-st_proc = '05'.

      UPDATE zlest0108
         SET st_proc = wa_saida_aviso-st_proc
             nro_nf_mdfe = ''
       WHERE vbeln = wa_saida_aviso-vbeln
         AND ebeln = wa_saida_aviso-ebeln.

      PERFORM f_estorno_custo CHANGING wa_saida_aviso.

      MODIFY it_saida_aviso FROM wa_saida_aviso INDEX wa_sel_rows-index TRANSPORTING   dacte fatserv ovserv transp doccus icon damdfe.
      CALL METHOD wa_alv_aviso_rec->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ELSE.
      MESSAGE 'Dacte deve ser não autorizada para este processo' TYPE  'I'.
    ENDIF.
  ELSE.
    MESSAGE 'Não existe documentos de transporte para estorno' TYPE  'I'.
  ENDIF.

ENDFORM.                    " SOL_ESTORNO_CTE

*&---------------------------------------------------------------------*
*&      Form  HANDLER_0101_CAT_HOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM handler_0101_cat_hot  USING    e_row_id     TYPE  lvc_s_row
                                    e_column_id  TYPE  lvc_s_col
                                    es_row_no    TYPE  lvc_s_roid.

  READ TABLE it_saida_aviso INTO wa_saida_aviso INDEX e_row_id-index.

  CHECK sy-subrc = 0.

  IF e_column_id = 'ICON'.
    IF wa_saida_aviso-icon = icon_led_red.

      SELECT *
        FROM zlest0111
        INTO TABLE it_zlest0111
        WHERE vbeln = wa_saida_aviso-vbeln
          AND ebeln = wa_saida_aviso-ebeln.

      IF it_zlest0111[] IS NOT INITIAL.
        PERFORM montar_layout_log.
        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            it_fieldcat           = estrutura[]
            i_save                = 'A'
            i_screen_start_column = 3
            i_screen_start_line   = 3
            i_screen_end_column   = 100
            i_screen_end_line     = 13
          TABLES
            t_outtab              = it_zlest0111.
      ENDIF.

    ENDIF.

  ELSEIF e_column_id = 'TRANSP'.

    IF wa_saida_aviso-transp = icon_execute_object.

      CLEAR: wa_zlest0108.
      SELECT SINGLE *
        FROM zlest0108
        INTO wa_zlest0108
       WHERE ebeln = wa_saida_aviso-ebeln
         AND vbeln = wa_saida_aviso-vbeln.

      IF wa_zlest0108-fknum GT 0.
        MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
        EXIT.
      ENDIF.

      IF wa_zlest0108-fat_contingencia_ecc = abap_true.
        SELECT SINGLE *
          FROM tvarvc INTO @DATA(lwa_user_fat)
         WHERE name = 'FAT_CONTINGENCIA_GOLIVE_US'
           AND low  = @sy-uname.

        IF sy-subrc NE 0.
          MESSAGE 'Faturamento Contingencia ECC! Operação não permitida!' TYPE 'I'.
          EXIT.
        ENDIF.

      ENDIF.

      IF sy-tcode NE 'ZLES0113'.
        MESSAGE 'Transação apenas de visualização' TYPE 'I'.
        EXIT.
      ENDIF.

      "187202 - bug solto - RGA
      SELECT COUNT(*)
         FROM lfb1
         WHERE lifnr = wa_zlest0108-agente_frete
           AND bukrs = p_bukrs-low
           AND sperr = ''
           AND loevm = ''.
      IF sy-subrc NE 0.
        CONCATENATE 'Fornecedor' wa_dados_transp-agente_frete 'não expandido para empresa.' wa_ekko-bukrs INTO DATA(vmessa) SEPARATED BY space.
        MESSAGE vmessa TYPE 'I'.
        EXIT.
      ENDIF.
      "187202 - bug solto - RGA - FIM

      UPDATE zlest0108 SET st_proc = wa_saida_aviso-st_proc
       WHERE ebeln = wa_saida_aviso-ebeln
         AND vbeln = wa_saida_aviso-vbeln.

      CLEAR: wl_erro, v_tknum.

      PERFORM f_gerar_vt USING wa_saida_aviso wa_zlest0108  CHANGING wl_erro.

      IF wl_erro EQ 'N' AND v_tknum IS NOT INITIAL.

        wa_saida_aviso-transp = v_tknum.

        MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING transp.

        CALL METHOD wa_alv_aviso_rec->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

        UPDATE zlest0108 SET doc_transp = wa_saida_aviso-transp
                             st_proc     = '04' " doc transporte
          WHERE ebeln = wa_saida_aviso-ebeln
            AND vbeln = wa_saida_aviso-vbeln.

        wa_saida_aviso-st_proc = '04'.

        MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING st_proc.

        DATA(lva_data_mov_custo) = sy-datum.

        IF wa_zlest0108-fat_contingencia_ecc EQ abap_true.

          IF wa_zlest0108-data_vf_frete IS NOT INITIAL.
            lva_data_mov_custo = wa_zlest0108-data_vf_frete.
          ELSEIF wa_zlest0108-data_custo IS NOT INITIAL..
            lva_data_mov_custo = wa_zlest0108-data_custo.
          ENDIF.

        ENDIF.

        PERFORM memorizar_dt_movimento_badi USING lva_data_mov_custo. "WA_SAIDA-DT_MOVIMENTO

        DATA: i_tknum	TYPE tknum.

        i_tknum = wa_saida_aviso-transp.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = i_tknum
          IMPORTING
            output = i_tknum.

        TRY .
            zcl_faturamento=>zif_faturamento~get_instance(
              )->get_processo_emissao_docs(
                EXPORTING
                  i_tknum       = i_tknum
                IMPORTING
                  e_doc_custo    = DATA(e_doc_custo)
                  e_conhecimento = DATA(e_conhecimento)
                  e_tipo_veiculo = DATA(e_tipo_veiculo)
                  e_tp_frete     = DATA(e_tp_frete)
                  e_manifesto    = DATA(e_manifesto)
              ).

          CATCH zcx_faturamento INTO DATA(ex_faturamento).
            MESSAGE ex_faturamento->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          CATCH zcx_error INTO DATA(ex_error).
            MESSAGE ex_error->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
        ENDTRY.

        IF e_tipo_veiculo EQ zif_faturamento=>st_tp_prop_veiculo_proprio.
          DATA(ckfprop) = abap_true.
        ELSE.
          ckfprop = abap_false.
        ENDIF.

        TRY .
            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = wa_saida_aviso-agente_frete
              )->ck_parceiro_local_negocio(
              ).

            DATA(rb_cus) = abap_false.

          CATCH zcx_parceiros.    " .
            IF ckfprop EQ abap_true.
              rb_cus = abap_true.
            ELSEIF ckfprop EQ abap_false AND e_tp_frete EQ zif_carga=>st_tp_frete_cpt.
              rb_cus = abap_true.
            ENDIF.
        ENDTRY.

        "Gerar custo
        "IF WA_SAIDA_AVISO-INCO1 = 'CPT'.
        SUBMIT zlesr0013 WITH so_tknum = wa_saida_aviso-transp
                         WITH p_vbeln  = wa_saida_aviso-vbeln
                         WITH p_ebeln  = wa_saida_aviso-ebeln
                         WITH cksetap  = abap_true
                         WITH rb_in    = e_doc_custo    "Documento de custo de frete
                         WITH rb_out   = e_conhecimento "Ordem / Fatura Serviço
                         WITH ckfprop  = ckfprop
                         WITH rb_cus   = rb_cus
                         WITH rb_dtfat = lva_data_mov_custo AND RETURN.

*        "Gerar custo
*        IF WA_SAIDA_AVISO-INCO1 = 'CPT'.
*          SUBMIT ZLESR0013 WITH SO_TKNUM = WA_SAIDA_AVISO-TRANSP
*                           WITH P_VBELN  = WA_SAIDA_AVISO-VBELN
*                           WITH P_EBELN  = WA_SAIDA_AVISO-EBELN
*                           WITH RB_OUT   = ''
*                           WITH RB_CUS   = 'X'
*                           WITH RB_DTFAT = SY-DATUM "WA_SAIDA-DT_MOVIMENTO
*          AND RETURN.
*        ELSE.
*          SUBMIT ZLESR0013 WITH SO_TKNUM = WA_SAIDA_AVISO-TRANSP
*                           WITH P_VBELN  = WA_SAIDA_AVISO-VBELN
*                           WITH P_EBELN  = WA_SAIDA_AVISO-EBELN
*                           WITH RB_DTFAT = SY-DATUM "WA_SAIDA-DT_MOVIMENTO
*          AND RETURN.
*        ENDIF.

        CLEAR: vl_fknum,vl_ov_frete,vl_fatura_frete.
        GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_fknum.
        GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_ov_frete.
        GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fatura_frete.

        IF vl_fknum IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = vl_fknum
            IMPORTING
              output = vl_fknum.

          wa_saida_aviso-doccus  = vl_fknum.

          IF e_conhecimento EQ abap_true.
            wa_saida_aviso-ovserv  = vl_ov_frete.
            wa_saida_aviso-fatserv = vl_fatura_frete.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_saida_aviso-fatserv
              IMPORTING
                output = wa_saida_aviso-fatserv.
          ENDIF.

          MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING doccus ovserv fatserv.

          IF vl_fatura_frete IS NOT INITIAL.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = vl_fknum
              IMPORTING
                output = vl_fknum.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = vl_ov_frete
              IMPORTING
                output = vl_ov_frete.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = vl_fatura_frete
              IMPORTING
                output = vl_fatura_frete.

            UPDATE zlest0108 SET st_proc      = '07' " Fatura Frete
                                 fknum        = vl_fknum
                                 ov_frete     = vl_ov_frete
                                 fatura_frete = vl_fatura_frete
              WHERE ebeln = wa_saida_aviso-ebeln
                AND vbeln = wa_saida_aviso-vbeln.

            wa_saida_aviso-st_proc = '07'.

            SELECT SINGLE j_1bnfdoc~bukrs, j_1bnflin~docnum
                FROM j_1bnflin
                INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
                INTO @DATA(wa_doc)
                WHERE j_1bnflin~refkey EQ @vl_fatura_frete.

            IF sy-subrc IS NOT INITIAL.
              WAIT UP TO 2 SECONDS.

              SELECT SINGLE j_1bnfdoc~bukrs, j_1bnflin~docnum
                  FROM j_1bnflin
                  INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
                  INTO @wa_doc
                  WHERE j_1bnflin~refkey EQ @vl_fatura_frete.
            ENDIF.

            IF sy-subrc IS INITIAL.
              wa_saida_aviso-dacte = icon_execute_object.
            ENDIF.

            MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING st_proc dacte.

          ELSEIF vl_ov_frete IS NOT INITIAL.
            UPDATE zlest0108 SET st_proc     = '06' " OV_FRETE
                                 fknum        = vl_fknum
                                 ov_frete     = vl_ov_frete
                                 fatura_frete = vl_fatura_frete
             WHERE ebeln = wa_saida_aviso-ebeln
               AND vbeln = wa_saida_aviso-vbeln.
            wa_saida_aviso-st_proc = '06'.

            MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING st_proc.

          ELSEIF vl_fknum IS NOT INITIAL.
            UPDATE zlest0108 SET st_proc      = '05' " Doc.Custo
                                 fknum        = vl_fknum
                                 ov_frete     = vl_ov_frete
                                 fatura_frete = vl_fatura_frete
             WHERE ebeln = wa_saida_aviso-ebeln
               AND vbeln = wa_saida_aviso-vbeln.

            wa_saida_aviso-st_proc = '05'.

            MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING st_proc.

            IF e_conhecimento EQ abap_false.

              "Verificar se Nota Eletrônica """"""""""""""""""""""""""""""""""""""""""""""""""
              IF e_manifesto EQ abap_true.
                SELECT * INTO TABLE @DATA(it_zlest0110)
                  FROM zlest0110
                 WHERE vbeln EQ @wa_saida_aviso-vbeln
                   AND chave NE @space.

                IF sy-subrc IS NOT INITIAL.
                  e_manifesto = abap_false.
                ENDIF.
              ENDIF.
              """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              CASE e_manifesto.
                WHEN abap_true.

                  CLEAR wa_saida_aviso-icon.
                  wa_saida_aviso-dacte    = icon_icon_list.
                  wa_saida_aviso-damdfe   = icon_execute_object.

                  MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING  dacte icon st_proc damdfe.

                  CALL METHOD wa_alv_aviso_rec->refresh_table_display
                    EXPORTING
                      is_stable = wa_stable.

                WHEN abap_false.

                  UPDATE zlest0108 SET st_proc     = '99' " Finalizado
                   WHERE ebeln = wa_saida_aviso-ebeln
                     AND vbeln = wa_saida_aviso-vbeln.

                  CLEAR wa_saida_aviso-icon.
                  wa_saida_aviso-dacte    = icon_icon_list.
                  wa_saida_aviso-damdfe   = icon_icon_list.
                  wa_saida_aviso-st_proc = '99'.

                  MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING  dacte icon st_proc damdfe.

                  CALL METHOD wa_alv_aviso_rec->refresh_table_display
                    EXPORTING
                      is_stable = wa_stable.

              ENDCASE.

            ENDIF.

          ENDIF.
          "Refresh na tela
          CALL METHOD wa_alv_aviso_rec->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.

      ELSE.
        MESSAGE 'Erro ao gerar transporte!' TYPE 'I'.
      ENDIF.

    ELSEIF wa_saida_aviso-transp NE icon_icon_list AND wa_saida_aviso-transp+0(4) NE '@11@'.

      SET PARAMETER ID 'TNR' FIELD wa_saida_aviso-transp+0(10).
      CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.

    ENDIF.

  ELSEIF e_column_id = 'DOCCUS'.
    IF wa_saida_aviso-doccus = icon_icon_list.
    ELSE.
      SET PARAMETER ID 'FKK'    FIELD wa_saida_aviso-doccus.
      CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
    ENDIF.
  ELSEIF  e_column_id = 'OVSERV'.
    IF wa_saida_aviso-ovserv  NE icon_icon_list.
      SET PARAMETER ID 'AUN'    FIELD wa_saida_aviso-ovserv.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ENDIF.
  ELSEIF  e_column_id = 'FATSERV'.
    IF wa_saida_aviso-fatserv  NE icon_icon_list.
      SET PARAMETER ID 'VF'   FIELD wa_saida_aviso-fatserv.
      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
    ENDIF.

  ELSEIF e_column_id = 'DACTE'.

    IF wa_saida_aviso-dacte = icon_execute_object. " Executar

      CLEAR wa_zlest0108.
      SELECT SINGLE *
        FROM zlest0108
        INTO wa_zlest0108
       WHERE vbeln = wa_saida_aviso-vbeln
         AND ebeln = wa_saida_aviso-ebeln.

      IF wa_zlest0108-nro_nf_frete GT 0.
        MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
        EXIT.
      ENDIF.

      IF wa_zlest0108-fat_contingencia_ecc EQ abap_true.

        DATA: lva_ok          TYPE  char01,
              lva_msg_retorno TYPE  string.

        CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
          EXPORTING
            i_vbeln          = wa_zlest0108-vbeln
            i_check_frete_ok = abap_true
          IMPORTING
            e_ok             = lva_ok
            e_msg_retorno    = lva_msg_retorno.

        IF lva_ok = abap_false.
          MESSAGE lva_msg_retorno TYPE 'I'.
          RETURN.
        ENDIF.

      ENDIF.

      IF sy-tcode NE 'ZLES0113'.
        MESSAGE 'Transação apenas de visualização' TYPE 'I'.
        EXIT.
      ENDIF.

      IF wa_saida_aviso-fatserv = icon_icon_list.
        MESSAGE 'Gerar a Fatura Frete!' TYPE 'I'.
      ELSE.

        UPDATE zlest0108 SET st_proc  = wa_saida_aviso-st_proc
              WHERE vbeln = wa_saida_aviso-vbeln
                AND ebeln = wa_saida_aviso-ebeln.

        REFRESH it_color.
        MOVE 'REMESSA'   TO wa_color-fname.
        MOVE '5'         TO wa_color-color-col.
        MOVE '1'         TO wa_color-color-int.
        MOVE '1'         TO wa_color-color-inv.
        APPEND wa_color TO it_color.
        wa_saida_aviso-color_cell[] = it_color[].
        MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING color_cell line_color .

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_saida_aviso-fatserv
          IMPORTING
            output = wa_saida_aviso-fatserv.

        SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
          FROM j_1bnflin
          INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
          INTO (vl_bukrs,vl_docnum)
          WHERE j_1bnflin~refkey = wa_saida_aviso-fatserv.


        SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_docnum.
        SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_bukrs.
        CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
        GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida_aviso-dacte.
        IF wa_saida_aviso-dacte = icon_complete.
          wa_saida_aviso-dacte = vl_docnum.
          CLEAR wa_saida_aviso-icon.
          "Apaga Log Erros
          " DELETE FROM ZLEST0111
          "   WHERE VBELN = WA_SAIDA_AVISO-VBELN
          "     AND EBELN = WA_SAIDA_AVISO-EBELN.
          MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING dacte icon.
          CALL METHOD wa_alv_aviso_rec->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

          UPDATE zlest0108 SET nro_nf_frete = wa_saida_aviso-dacte
                              st_proc      = '99' " Fim
           WHERE vbeln = wa_saida_aviso-vbeln
             AND ebeln = wa_saida_aviso-ebeln.

          wa_saida_aviso-st_proc = '99'.
          MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING st_proc.

        ELSE.
          MESSAGE 'Dacte ainda não autorizado pela SEFAZ' TYPE 'I'.
        ENDIF.

      ENDIF.
    ELSEIF wa_saida_aviso-dacte NE icon_icon_list.
      SELECT SINGLE bukrs
        FROM j_1bnfdoc
        INTO vl_bukrs
        WHERE docnum = wa_saida_aviso-dacte.
      SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida_aviso-dacte.
      SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_bukrs.
      CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
    ENDIF.


  ELSEIF e_column_id = 'VBELN'.
    IF wa_saida_aviso-vbeln  NE icon_icon_list.
      SET PARAMETER ID 'VL'  FIELD wa_saida_aviso-vbeln.
      SET PARAMETER ID 'VLM' FIELD wa_saida_aviso-vbeln.
      SET PARAMETER ID 'VLG' FIELD wa_saida_aviso-vbeln.
      CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
    ENDIF.

  ELSEIF e_column_id = 'DAMDFE'.

    IF wa_saida_aviso-damdfe = icon_execute_object. " Executar

      CLEAR wa_zlest0108.
      SELECT SINGLE *
        FROM zlest0108

        INTO wa_zlest0108
       WHERE vbeln = wa_saida_aviso-vbeln
         AND ebeln = wa_saida_aviso-ebeln.

      IF wa_zlest0108-nro_nf_mdfe GT 0.
        MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
        EXIT.
*** Inicio - Rubenilson Pereira - 05.08.25 #133798
      ELSE.
        SELECT *
          FROM zlest0110
          INTO @DATA(ls_0110)
          UP TO 1 ROWS
          WHERE vbeln = @wa_zlest0108-vbeln.
        ENDSELECT.
        IF sy-subrc IS INITIAL.
          SELECT *
            FROM zsdt0241
            INTO @DATA(ls_0241)
            UP TO 1 ROWS
            WHERE chave = @ls_0110-chave.
          ENDSELECT.
          IF sy-subrc IS INITIAL.
            IF ls_0241-docnum IS NOT INITIAL.
              SELECT *
                FROM zsdt0102
                INTO @DATA(ls_0102)
                UP TO 1 ROWS
                WHERE docnum = @ls_0241-docnum
                  AND autorizado = @abap_true
                  AND estornado  = @space
                  AND cancel     = @space.
              ENDSELECT.
              IF sy-subrc IS INITIAL.
                UPDATE zlest0108 SET nro_nf_mdfe = ls_0241-docnum
                                     st_proc     = '99'
                 WHERE vbeln = wa_saida_aviso-vbeln
                   AND ebeln = wa_saida_aviso-ebeln.

                wa_saida_aviso-st_proc = '99'.
                wa_saida_aviso-damdfe  = ls_0241-docnum.
                MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING st_proc damdfe.

                DATA(lv_existe) = abap_true.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
*** Fim - Rubenilson Pereira - 05.08.25 #133798
      ENDIF.

      IF lv_existe IS INITIAL." Rubenilson Pereira - 05.08.25 #133798

        IF sy-tcode NE 'ZLES0113'.
          MESSAGE 'Transação apenas de visualização' TYPE 'I'.
          EXIT.
        ENDIF.

        IF wa_saida_aviso-transp = icon_icon_list.
          MESSAGE 'Gerar o Documento de Transporte!' TYPE 'I'.
        ELSE.

          i_tknum = wa_saida_aviso-transp.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = i_tknum
            IMPORTING
              output = i_tknum.

          TRY .
              zcl_faturamento=>zif_faturamento~get_instance(
                )->get_processo_emissao_docs(
                  EXPORTING
                    i_tknum       = i_tknum
                  IMPORTING
                    e_conhecimento = e_conhecimento
                    e_manifesto    = e_manifesto
                ).

            CATCH zcx_faturamento INTO ex_faturamento.
              MESSAGE ex_faturamento->get_longtext( ) TYPE 'I'.
              EXIT.
            CATCH zcx_error INTO ex_error.
              MESSAGE ex_error->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
          ENDTRY.

          IF e_conhecimento EQ abap_true.
            MESSAGE 'Gerar MDF-e pela CT-e!' TYPE 'I'.
          ELSEIF e_manifesto EQ abap_true.

            "Verificar se Nota Eletrônica """"""""""""""""""""""""""""""""""""""""""""""""""
            IF e_manifesto EQ abap_true.
              SELECT * INTO TABLE @it_zlest0110
                FROM zlest0110
               WHERE vbeln EQ @wa_saida_aviso-vbeln
                 AND chave NE @space.

              IF sy-subrc IS NOT INITIAL.
                e_manifesto = abap_false.
              ENDIF.
            ENDIF.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            IF e_manifesto EQ abap_false.
              MESSAGE 'MDF-e Somente gera para NF-e!' TYPE 'I'.
            ELSE.
              TRY .
                  DATA(at_aviso_recebimento) =
                    zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_instance(
                      )->set_registro( EXPORTING i_vbeln = CONV #( wa_saida_aviso-vbeln )
                      )->set_gerar_mdfe_avulsa(
                      )->at_aviso_recebimento.

                  wa_saida_aviso-st_proc = at_aviso_recebimento-st_proc.
                  wa_saida_aviso-damdfe  = at_aviso_recebimento-nro_nf_mdfe.
                  MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING st_proc damdfe.

                CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_frete).    "

                  wa_saida_aviso-st_proc = at_aviso_recebimento-st_proc.
                  wa_saida_aviso-damdfe  = at_aviso_recebimento-nro_nf_mdfe.
                  MODIFY it_saida_aviso FROM wa_saida_aviso INDEX e_row_id-index TRANSPORTING st_proc damdfe.

                  ex_frete->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
              ENDTRY.
            ENDIF.

          ENDIF.

          "Refresh na tela
          CALL METHOD wa_alv_aviso_rec->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.
      ENDIF.

    ELSEIF wa_saida_aviso-damdfe NE icon_icon_list.
      SELECT SINGLE bukrs
        FROM j_1bnfdoc
        INTO vl_bukrs
        WHERE docnum = wa_saida_aviso-damdfe.
      SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida_aviso-damdfe.
      SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_bukrs.
      CALL TRANSACTION 'ZMDFE' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDIF.


ENDFORM.                    " HANDLER_0101_CAT_HOT
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_AVISO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM atualiza_aviso  USING p_wa_saida TYPE ty_saida.

  DATA: v_cont_fre TYPE i,
        v_agregado TYPE zlest0002-agregado.

  REFRESH: it_saida_aviso, it_zlest0108_aux.

*  CHECK wa_alv_aviso_rec IS NOT INITIAL.

*-CS2022001149-29.03.2023-#103469-JT-inicio
*-----------------------
* monta range data
*-----------------------
  FREE: r_eindt.

  IF vg_eindt_ini IS NOT INITIAL OR
     vg_eindt_fim IS NOT INITIAL.

    IF vg_eindt_fim IS INITIAL.
      r_eindt-sign   = 'I'.
      r_eindt-option = 'EQ'.
      r_eindt-low    = vg_eindt_ini.
      r_eindt-high   = '00000000'.
    ELSE.
      r_eindt-sign   = 'I'.
      r_eindt-option = 'BT'.
      r_eindt-low    = vg_eindt_ini.
      r_eindt-high   = vg_eindt_fim.
    ENDIF.

    APPEND r_eindt.

    SELECT vbeln, vbelp, ebeln
      INTO TABLE @DATA(t_ekes)
      FROM ekes
     WHERE ebeln  = @p_wa_saida-ebeln
       AND eindt IN @r_eindt.

    IF sy-subrc = 0.
      SELECT *
       INTO TABLE it_zlest0108_aux
       FROM zlest0108
        FOR ALL ENTRIES IN t_ekes
      WHERE vbeln = t_ekes-vbeln.
    ENDIF.
  ELSE.
    SELECT *
     INTO TABLE it_zlest0108_aux
     FROM zlest0108
    WHERE ebeln = p_wa_saida-ebeln.
  ENDIF.
*-CS2022001149-29.03.2023-#103469-JT-fim

  LOOP AT it_zlest0108_aux INTO wa_zlest0108_aux.

    SELECT SINGLE * INTO wa_likp FROM likp WHERE vbeln = wa_zlest0108_aux-vbeln.

    IF ( sy-subrc NE 0 ).
      CONTINUE.
    ENDIF.

    CLEAR: wa_saida_aviso.

    wa_saida_aviso-st_proc       = wa_zlest0108_aux-st_proc.
    wa_saida_aviso-bsart         = p_wa_saida-bsart.
    wa_saida_aviso-ebeln         = wa_zlest0108_aux-ebeln.
    wa_saida_aviso-lifnr         = p_wa_saida-lifnr.
    wa_saida_aviso-matnr         = p_wa_saida-matnr.
    wa_saida_aviso-ebelp         = p_wa_saida-ebelp.
    wa_saida_aviso-werks         = p_wa_saida-werks.
    wa_saida_aviso-branch        = p_wa_saida-branch.
    wa_saida_aviso-lgort         = p_wa_saida-lgort.
    wa_saida_aviso-charg         = p_wa_saida-charg.

    IF wa_likp-lifex IS NOT INITIAL.
      wa_saida_aviso-verur         = wa_likp-lifex.
    ENDIF.

    wa_saida_aviso-vbeln         = wa_likp-vbeln.
    wa_saida_aviso-btgew         = wa_likp-btgew.
    wa_saida_aviso-erdat         = wa_likp-erdat.
    wa_saida_aviso-route         = wa_likp-route.
    wa_saida_aviso-inco1         = wa_likp-inco1.
    wa_saida_aviso-agente_frete  = wa_zlest0108_aux-agente_frete.
    wa_saida_aviso-placa_cav     = wa_zlest0108_aux-placa_cav.

    CLEAR: wa_zsdt0011.
    SELECT SINGLE * INTO wa_zsdt0011 FROM zsdt0011 WHERE bsart = wa_saida_aviso-bsart.

    IF sy-subrc = 0.
      wa_saida_aviso-shtyp = wa_zsdt0011-shtyp.
    ENDIF.

    "Agregado
    CLEAR: v_agregado.
    SELECT SINGLE agregado
      FROM zlest0002
      INTO v_agregado
     WHERE pc_veiculo = wa_zlest0108_aux-placa_cav.

    IF sy-subrc = 0.
      IF v_agregado = 1.
        wa_saida_aviso-add01 = '0000000001'.
      ELSE.
        wa_saida_aviso-add01 = '0000000002'.
      ENDIF.
    ENDIF.

    PERFORM f_atual_frete USING wa_zlest0108_aux CHANGING wa_saida_aviso.

    "Substituir valor do frete pelo valor historico
    IF wa_zlest0108_aux-kbetr GT 0.
      wa_saida_aviso-kbetr = wa_zlest0108_aux-kbetr.
      v_cont_fre = 1.
    ENDIF.

    IF ( wa_zlest0108_aux-doc_transp IS INITIAL ) OR
       ( wa_zlest0108_aux-doc_transp = '' ).
      wa_saida_aviso-transp          = icon_execute_object.
    ELSE.
      wa_saida_aviso-transp          = wa_zlest0108_aux-doc_transp.

      TRY .
          zcl_faturamento=>zif_faturamento~get_instance(
            )->get_processo_emissao_docs(
              EXPORTING
                i_tknum       = wa_zlest0108_aux-doc_transp
              IMPORTING
                e_conhecimento = DATA(e_conhecimento)
                e_manifesto    = DATA(e_manifesto)
            ).

        CATCH zcx_faturamento INTO DATA(ex_faturamento).
        CATCH zcx_error INTO DATA(ex_error).
      ENDTRY.

    ENDIF.

    IF ( wa_zlest0108_aux-fknum IS INITIAL ) OR
       ( wa_zlest0108_aux-fknum = '' ).
      wa_saida_aviso-doccus          = icon_icon_list.
    ELSE.
      wa_saida_aviso-doccus          = wa_zlest0108_aux-fknum.
    ENDIF.

    "Verificar se Nota Eletrônica """"""""""""""""""""""""""""""""""""""""""""""""""
    IF e_manifesto EQ abap_true.
      SELECT * INTO TABLE @DATA(it_zlest0110)
        FROM zlest0110
       WHERE vbeln EQ @wa_zlest0108_aux-vbeln
         AND chave NE @space.

      IF sy-subrc IS NOT INITIAL.
        e_manifesto = abap_false.
      ENDIF.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF e_conhecimento EQ abap_true.

      wa_saida_aviso-damdfe = icon_icon_list.

      IF ( wa_zlest0108_aux-ov_frete IS INITIAL ) OR
         ( wa_zlest0108_aux-ov_frete = '' ).
        wa_saida_aviso-ovserv          = icon_icon_list.
      ELSE.
        wa_saida_aviso-ovserv          = wa_zlest0108_aux-ov_frete.
      ENDIF.

      CLEAR vl_docnum.
      IF ( wa_zlest0108_aux-fatura_frete IS INITIAL ) OR
         ( wa_zlest0108_aux-fatura_frete = '' ).
        wa_saida_aviso-fatserv         = icon_icon_list.
      ELSE.
        wa_saida_aviso-fatserv         = wa_zlest0108_aux-fatura_frete.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_saida_aviso-fatserv
          IMPORTING
            output = wa_saida_aviso-fatserv.

        IF ( wa_zlest0108_aux-nro_nf_frete IS INITIAL ) OR
           ( wa_zlest0108_aux-nro_nf_frete = '' ).

          SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
              FROM j_1bnflin
              INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
              INTO (vl_bukrs,vl_docnum)
              WHERE j_1bnflin~refkey = wa_saida_aviso-fatserv.

          IF sy-subrc = 0.
            SELECT SINGLE docnum
             FROM j_1bnfe_active
             INTO v_docnum
             WHERE docnum     = vl_docnum
             AND   cancel     = ''
             AND   docsta     = '1'.

            IF sy-subrc = 0.
              wa_zlest0108_aux-nro_nf_frete = vl_docnum.
              UPDATE zlest0108 SET st_proc      = '99'
                                   nro_nf_frete = vl_docnum
                WHERE vbeln = wa_zlest0108_aux-vbeln
                  AND ebeln = wa_zlest0108_aux-ebeln.

              wa_saida_aviso-st_proc = '99'.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

      IF ( wa_zlest0108_aux-nro_nf_frete IS INITIAL ) OR
         ( wa_zlest0108_aux-nro_nf_frete = '' ).
        wa_saida_aviso-dacte             = icon_execute_object.
      ELSE.
        wa_saida_aviso-dacte             = wa_zlest0108_aux-nro_nf_frete.
      ENDIF.

    ELSEIF e_manifesto EQ abap_true.

      IF ( wa_zlest0108_aux-nro_nf_mdfe IS INITIAL ) OR
         ( wa_zlest0108_aux-nro_nf_mdfe = '' ).
        wa_saida_aviso-damdfe = icon_execute_object.
      ELSE.
        wa_saida_aviso-damdfe = wa_zlest0108_aux-nro_nf_mdfe.

        IF sy-subrc = 0.
          SELECT SINGLE docnum
           FROM j_1bnfe_active
           INTO v_docnum
           WHERE docnum     = wa_saida_aviso-damdfe
           AND   cancel     = ''
           AND   docsta     = '1'.

          IF sy-subrc = 0.
            UPDATE zlest0108 SET st_proc      = '99'
              WHERE vbeln = wa_zlest0108_aux-vbeln
                AND ebeln = wa_zlest0108_aux-ebeln.
            wa_saida_aviso-st_proc = '99'.
          ENDIF.
        ENDIF.

      ENDIF.
    ELSE.
      wa_saida_aviso-damdfe = icon_icon_list.
    ENDIF.

    IF ( wa_zlest0108_aux-st_proc = '99' ).
      IF wa_saida_aviso-transp          = icon_execute_object.
        wa_saida_aviso-transp          = icon_icon_list.
      ENDIF.
      IF wa_saida_aviso-dacte          = icon_execute_object.
        wa_saida_aviso-dacte          = icon_icon_list.
      ENDIF.
      IF wa_saida_aviso-damdfe         = icon_execute_object.
        wa_saida_aviso-damdfe          = icon_icon_list.
      ENDIF.
    ENDIF.

    "Checar se há cancelamento de CTE e estorna documentos
    "DACTE
    IF wa_saida_aviso-dacte NE icon_execute_object
      AND wa_saida_aviso-dacte NE icon_icon_list
      AND wa_saida_aviso-dacte IS NOT INITIAL
      AND wa_saida_aviso-dacte NE ''
      AND wa_saida_aviso-inco1 EQ 'CIF'
      AND wa_saida_aviso-st_proc = '99'. " no automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE

      SELECT SINGLE docnum
        FROM j_1bnfe_active
        INTO v_docnum
       WHERE docnum   = wa_saida_aviso-dacte "NRO_NF_FRETE
         AND cancel   = 'X'.

      IF sy-subrc EQ 0.
        REFRESH it_zlest0111.

        "Lima CT-e
        wa_saida_aviso-dacte  = icon_execute_object.

        UPDATE zlest0108 SET st_proc      = '07'
                             nro_nf_frete = ''
         WHERE ebeln = wa_saida_aviso-ebeln
           AND vbeln = wa_saida_aviso-vbeln.

        COMMIT WORK.

        wa_saida_aviso-st_proc = '07'.
        PERFORM f_estorno_cte CHANGING wa_saida_aviso.

        IF it_zlest0111[] IS NOT INITIAL.
          wa_saida_aviso-icon = icon_led_red.
        ELSE.
          CLEAR wa_saida_aviso-icon.
        ENDIF.
      ENDIF.

    ENDIF.

    APPEND wa_saida_aviso TO it_saida_aviso.

    SORT it_saida_aviso DESCENDING BY vbeln.

  ENDLOOP.

  CHECK wa_alv_aviso_rec IS NOT INITIAL.

  CALL METHOD wa_alv_aviso_rec->refresh_table_display
    EXPORTING
      is_stable = wa_stable.


ENDFORM.                    " ATUALIZA_AVISO
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_PASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_pass .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'WA_QTDE_AVISO-PESO_BRUTO1' OR
           'WA_QTDE_AVISO-PESO_TARA1'  OR
           'WA_QTDE_AVISO-PESO_BRUTO2' OR
           'WA_QTDE_AVISO-PESO_TARA2'.
        screen-invisible = '1'.
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_PASS


FORM atualiza_campos_peso1 .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXT_PESO1'OR
           'TXT_TARA1'.
        screen-active  = '1'.
        MODIFY SCREEN.
      WHEN 'WA_QTDE_AVISO-PESO_BRUTO1' OR
           'WA_QTDE_AVISO-PESO_TARA1'.
        screen-invisible = '1'.
        screen-active    = '1'.
        MODIFY SCREEN.
      WHEN 'TXT_PESO2'                 OR
           'TXT_TARA2'                 OR
           'WA_QTDE_AVISO-PESO_BRUTO2' OR
           'WA_QTDE_AVISO-PESO_TARA2'.
        screen-invisible = '1'.
        screen-active    = '0'.
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_PASS

FORM atualiza_campos_peso2 .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXT_PESO1'                 OR
           'TXT_TARA1'                 OR
           'WA_QTDE_AVISO-PESO_BRUTO1' OR
           'WA_QTDE_AVISO-PESO_TARA1'.
        screen-invisible = '1'.
        screen-active    = '0'.
        MODIFY SCREEN.
      WHEN 'TXT_PESO2' OR
           'TXT_TARA2'.
        screen-active    = '1'.
        MODIFY SCREEN.
      WHEN 'TXT_PESO2'                 OR
           'TXT_TARA2'                 OR
           'WA_QTDE_AVISO-PESO_BRUTO2' OR
           'WA_QTDE_AVISO-PESO_TARA2'.
        screen-invisible = '1'.
        screen-active    = '1'.
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_PASS


*&---------------------------------------------------------------------*
*&      Form  INFORMAR_QTDE_AVISO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM informar_qtde_aviso .

  IF vg_view_transp IS NOT INITIAL.
    MESSAGE 'Modo de visualização! Operação não permitida!' TYPE  'S'.
    EXIT.
  ENDIF.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD wa_alv_dados_aviso->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  IF lines( it_sel_rows ) NE 1.
    MESSAGE 'Selecione apenas um item!' TYPE  'S'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  CLEAR: wa_qtde_aviso, wa_dados_aviso, vg_det_peso1, vg_det_peso2.

  READ TABLE it_dados_aviso INTO wa_dados_aviso INDEX wa_sel_rows-index.

  CALL SCREEN 0107 STARTING AT 07 05 ENDING AT 75 08.


ENDFORM.                    " INFORMAR_QTDE_AVISO
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM conversion_exit_alpha.

  "NF --------------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_dados_nf-numero
    IMPORTING
      output = wa_dados_nf-numero.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_dados_nf-serie
    IMPORTING
      output = wa_dados_nf-serie.

  "Parceiros --------------------------------------------

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_dados_transp-agente_frete
    IMPORTING
      output = wa_dados_transp-agente_frete.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_dados_transp-cod_remetente
    IMPORTING
      output = wa_dados_transp-cod_remetente.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_dados_transp-cod_dest_merc
    IMPORTING
      output = wa_dados_transp-cod_dest_merc.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_dados_transp-cod_loc_coleta
    IMPORTING
      output = wa_dados_transp-cod_loc_coleta.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_dados_transp-cod_loc_entrega
    IMPORTING
      output = wa_dados_transp-cod_loc_entrega.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_dados_transp-motorista
    IMPORTING
      output = wa_dados_transp-motorista.


  "Dados Ordem Carregamento ----------------------

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_dados_transp-nro_ordem_car
    IMPORTING
      output = wa_dados_transp-nro_ordem_car.


ENDFORM.                    " CONVERSION_EXIT_ALPHA
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_screen_0105 .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'WA_DADOS_TRANSP-AGENTE_FRETE' OR
           'WA_DADOS_TRANSP-COD_REMETENTE' OR
           'WA_DADOS_TRANSP-COD_DEST_MERC' OR
           'WA_DADOS_TRANSP-COD_LOC_COLETA' OR
           'WA_DADOS_TRANSP-COD_LOC_ENTREGA'.

        IF vg_view_transp IS NOT INITIAL.
          screen-input  = '0'.
        ENDIF.

        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_SCREEN

FORM atualiza_screen_0104 .

  LOOP AT SCREEN.
    CASE screen-name(8).
      WHEN 'WA_DADOS' OR
           'CTE_TEXT'.

        IF vg_view_transp IS NOT INITIAL.
          screen-input  = '0'.
        ENDIF.

        MODIFY SCREEN.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_SCREEN

FORM recuperar_docs .

  DATA: var_answer TYPE c,
        wa_vbak    TYPE vbak,
        vl_vbeln   TYPE vbfa-vbeln,
        vl_mjahr   TYPE vbfa-mjahr,
        vl_docnum  TYPE j_1bnflin-docnum.

  CALL METHOD wa_alv_aviso_rec->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK NOT it_sel_rows IS INITIAL.

  IF sy-tcode NE 'ZLES0113'.
    MESSAGE 'Transação apenas de visualização!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF ( lines( it_sel_rows ) NE 1 ).
    MESSAGE 'Selecione apenas uma linha!' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida_aviso ASSIGNING FIELD-SYMBOL(<fs_out>) INDEX wa_sel_rows-index.

  CHECK sy-subrc = 0.

  CHECK ( <fs_out>-ebeln IS NOT INITIAL ) AND ( <fs_out>-vbeln IS NOT INITIAL ).

*-------------------------------------------------------------------------------------*
* ============>               DOC. TRANSPORTE                            <=========== *
*-------------------------------------------------------------------------------------*

  CLEAR: v_tknum.

  DATA(_delivery) = <fs_out>-vbeln.
  DATA(_vbtyp_v)  = '7'.

  IF ( <fs_out>-transp EQ icon_execute_object ) AND ( _delivery IS NOT INITIAL ).
    SELECT SINGLE vttk~tknum INTO v_tknum
      FROM vbfa INNER JOIN vttk ON  vttk~tknum = vbfa~vbeln
                                AND vttk~vsart = '01'
     WHERE vbfa~vbelv    = _delivery
       AND vbfa~vbtyp_n  = '8'
       AND vbfa~vbtyp_v  = _vbtyp_v.

    IF sy-subrc = 0.
      <fs_out>-transp = v_tknum.
      UPDATE zlest0108 SET doc_transp = v_tknum
       WHERE ebeln = <fs_out>-ebeln
         AND vbeln = <fs_out>-vbeln.
    ENDIF.

  ELSEIF ( <fs_out>-transp IS NOT INITIAL AND <fs_out>-transp(1) NE '@' ).
    SELECT SINGLE vttk~tknum INTO v_tknum
      FROM vbfa INNER JOIN vttk ON  vttk~tknum = vbfa~vbeln
                                AND vttk~vsart = '01'
     WHERE vbfa~vbelv = _delivery
       AND vbfa~vbtyp_n  = '8'
       AND vbfa~vbtyp_v  = _vbtyp_v.
    IF sy-subrc NE 0.
      <fs_out>-transp = icon_execute_object.
      UPDATE zlest0108 SET doc_transp = ''
        WHERE ebeln = <fs_out>-ebeln
          AND vbeln = <fs_out>-vbeln.
    ELSEIF <fs_out>-transp NE v_tknum.
      <fs_out>-transp = v_tknum.
      UPDATE zlest0108 SET doc_transp = v_tknum
       WHERE ebeln = <fs_out>-ebeln
         AND vbeln = <fs_out>-vbeln.
    ENDIF.
  ENDIF.

*-------------------------------------------------------------------------------------*
* ============>               DOC. CUSTO                                 <=========== *
*-------------------------------------------------------------------------------------*

  IF ( <fs_out>-doccus+0(1) = '@' ) AND
     ( <fs_out>-transp IS NOT INITIAL AND <fs_out>-transp(1) NE '@' ).

    SELECT SINGLE fknum
      FROM vfkp INTO vl_fknum
     WHERE rebel = <fs_out>-transp.

    IF sy-subrc = 0.
      <fs_out>-doccus = vl_fknum.

      UPDATE zlest0108 SET fknum = vl_fknum
       WHERE ebeln = <fs_out>-ebeln
         AND vbeln = <fs_out>-vbeln.
    ENDIF.
  ELSEIF ( <fs_out>-doccus IS NOT INITIAL AND <fs_out>-doccus(1) NE '@' ).

    SELECT SINGLE fknum
      FROM vfkp INTO vl_fknum
     WHERE rebel = <fs_out>-transp.
    IF sy-subrc NE 0.
      <fs_out>-doccus = icon_icon_list.
      UPDATE zlest0108 SET fknum = ''
       WHERE ebeln = <fs_out>-ebeln
         AND vbeln = <fs_out>-vbeln.
    ENDIF.
  ENDIF.

*-------------------------------------------------------------------------------------*
* ============>               OV. SERVIÇO                                <=========== *
*-------------------------------------------------------------------------------------*

  CLEAR: wa_vbak.

  IF ( <fs_out>-ovserv+0(1) = '@' ) AND
     ( <fs_out>-transp IS NOT INITIAL AND <fs_out>-transp(1) NE '@' ).

    SELECT SINGLE  vbeln auart  kunnr
      FROM vbak INTO CORRESPONDING FIELDS OF wa_vbak
     WHERE tknum = <fs_out>-transp.
    IF sy-subrc = 0.
      <fs_out>-ovserv = wa_vbak-vbeln.

      UPDATE zlest0108 SET ov_frete = wa_vbak-vbeln
        WHERE ebeln = <fs_out>-ebeln
          AND vbeln = <fs_out>-vbeln.
    ENDIF.

  ELSEIF ( <fs_out>-ovserv IS NOT INITIAL ) AND ( <fs_out>-ovserv(1) NE '@' ).

    SELECT SINGLE  vbeln auart  kunnr
      FROM vbak INTO CORRESPONDING FIELDS OF wa_vbak
     WHERE tknum = <fs_out>-transp.

    IF sy-subrc NE 0.
      <fs_out>-ovserv = icon_icon_list.

      UPDATE zlest0108 SET ov_frete = ''
        WHERE ebeln = <fs_out>-ebeln
          AND vbeln = <fs_out>-vbeln.
    ENDIF.
  ENDIF.


*-------------------------------------------------------------------------------------*
* ============>               FATURA. FRETE                              <=========== *
*-------------------------------------------------------------------------------------*

  IF ( <fs_out>-fatserv+0(1) = '@' ) AND
     ( <fs_out>-ovserv IS NOT INITIAL AND <fs_out>-ovserv(1) NE '@' ).
    SELECT SINGLE a~vbeln a~mjahr
      FROM vbfa AS a INTO (vl_vbeln,vl_mjahr)
     WHERE a~vbelv = <fs_out>-ovserv
       AND a~vbtyp_n  = 'M'
       AND a~vbtyp_v  = 'C'
       AND NOT EXISTS ( SELECT *
                          FROM vbfa AS b
                         WHERE b~vbelv   = a~vbeln
                           AND b~vbtyp_n = 'N' "estorno
                       ).
    IF sy-subrc = 0.
      <fs_out>-fatserv = vl_vbeln.

      UPDATE zlest0108 SET fatura_frete = vl_vbeln
       WHERE ebeln = <fs_out>-ebeln
         AND vbeln = <fs_out>-vbeln.

    ELSE.
      <fs_out>-dacte   = icon_execute_object.

      UPDATE zlest0108 SET nro_nf_frete = ''
       WHERE ebeln = <fs_out>-ebeln
         AND vbeln = <fs_out>-vbeln.
    ENDIF.

  ELSEIF ( <fs_out>-fatserv IS NOT INITIAL AND <fs_out>-fatserv(1) NE '@' ).
    SELECT SINGLE fksto
      FROM vbrk INTO vl_fksto
     WHERE vbeln = <fs_out>-fatserv.
    IF sy-subrc NE 0. "Fatura não existe
      <fs_out>-fatserv = icon_icon_list.

      UPDATE zlest0108 SET fatura_frete = ''
       WHERE ebeln = <fs_out>-ebeln
         AND vbeln = <fs_out>-vbeln.

    ELSE.
      SELECT SINGLE vbeln mjahr
        FROM vbfa INTO (vl_vbeln,vl_mjahr)
       WHERE vbelv = <fs_out>-fatserv
         AND vbtyp_n  = 'N'. "estorno
      IF sy-subrc = 0. "Achou estorno
        <fs_out>-fatserv = icon_icon_list.

        UPDATE zlest0108 SET fatura_frete = ''
         WHERE ebeln = <fs_out>-ebeln
           AND vbeln = <fs_out>-vbeln.
      ENDIF.
    ENDIF.
  ENDIF.


*-------------------------------------------------------------------------------------*
* ============>                   DACTE                                  <=========== *
*-------------------------------------------------------------------------------------*

  IF ( <fs_out>-dacte EQ icon_execute_object ) AND
     ( <fs_out>-fatserv IS NOT INITIAL AND <fs_out>-fatserv(1) NE '@' ).
    CLEAR: vl_docnum.

    SELECT SINGLE docnum
      FROM j_1bnflin INTO vl_docnum
     WHERE refkey = <fs_out>-fatserv.

    IF ( sy-subrc = 0 ) AND ( vl_docnum IS NOT INITIAL ).

      SELECT SINGLE docnum
        FROM j_1bnfe_active INTO v_docnum
       WHERE docnum EQ vl_docnum
         AND cancel EQ space
         AND scssta NE '2'
         AND docsta EQ '1'.

      IF sy-subrc = 0.
        <fs_out>-dacte  = vl_docnum.

        UPDATE zlest0108 SET nro_nf_frete = vl_docnum
         WHERE ebeln = <fs_out>-ebeln
           AND vbeln = <fs_out>-vbeln.
      ENDIF.
    ENDIF.

  ELSEIF ( <fs_out>-dacte   IS NOT INITIAL AND <fs_out>-dacte(1)   NE '@' ) AND
         ( <fs_out>-fatserv IS NOT INITIAL AND <fs_out>-fatserv(1) NE '@' ).

    CLEAR: vl_docnum.
    SELECT SINGLE docnum
      FROM j_1bnflin INTO vl_docnum
     WHERE refkey = <fs_out>-fatserv.

    SELECT SINGLE docnum
      FROM j_1bnfe_active INTO v_docnum
     WHERE docnum EQ vl_docnum
       AND cancel EQ 'X'.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE docnum
        FROM j_1bnfe_active INTO v_docnum
       WHERE docnum EQ vl_docnum
         AND scssta EQ '2'
         AND docsta EQ '1'.
    ENDIF.

    IF  ( sy-subrc = 0  AND vl_docnum IS NOT INITIAL ) OR ( vl_docnum NE <fs_out>-dacte ). "dacte cancelada  OU fatura não é da DACTE.
      <fs_out>-dacte  = icon_execute_object.
      UPDATE zlest0108 SET nro_nf_frete = ''
       WHERE ebeln = <fs_out>-ebeln
         AND vbeln = <fs_out>-vbeln.
    ENDIF.

  ELSEIF <fs_out>-fatserv EQ icon_execute_object.

    <fs_out>-dacte = icon_execute_object.

    UPDATE zlest0108 SET nro_nf_frete = ''
     WHERE ebeln = <fs_out>-ebeln
       AND vbeln = <fs_out>-vbeln.
  ENDIF.


*-------------------------------------------------------------------------------------*
* ============>                   MDFE                                  <=========== *
*-------------------------------------------------------------------------------------*

  IF ( <fs_out>-damdfe IS NOT INITIAL AND <fs_out>-damdfe(1)   NE '@' ) AND
     ( <fs_out>-transp IS NOT INITIAL AND <fs_out>-transp(1) NE '@' ).

    SELECT SINGLE docnum
      FROM j_1bnfe_active INTO v_docnum
     WHERE docnum EQ <fs_out>-damdfe
       AND cancel EQ 'X'.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE docnum
        FROM j_1bnfe_active INTO v_docnum
       WHERE docnum EQ <fs_out>-damdfe
         AND scssta EQ '2'
         AND docsta EQ '1'.
    ENDIF.

    IF  ( sy-subrc = 0  AND vl_docnum IS NOT INITIAL ) OR ( vl_docnum NE <fs_out>-damdfe ). "dacte cancelada  OU fatura não é da DACTE.
      <fs_out>-damdfe  = icon_execute_object.
      UPDATE zlest0108 SET nro_nf_mdfe = ''
       WHERE ebeln = <fs_out>-ebeln
         AND vbeln = <fs_out>-vbeln.
    ENDIF.

  ELSEIF <fs_out>-transp EQ icon_execute_object.

    <fs_out>-damdfe = icon_execute_object.

    UPDATE zlest0108 SET nro_nf_mdfe = ''
     WHERE ebeln = <fs_out>-ebeln
       AND vbeln = <fs_out>-vbeln.

  ENDIF.

*-------------------------------------------------------------------------------------*
* ============>          RECOMPOSIÇÃO DE STATUS.                         <=========== *
*-------------------------------------------------------------------------------------*

  IF ( <fs_out>-vbeln IS NOT INITIAL ) AND ( <fs_out>-vbeln(1) NE '@' ).
    <fs_out>-st_proc = '13'.
  ENDIF.

  IF ( <fs_out>-transp IS NOT INITIAL ) AND ( <fs_out>-transp(1) NE '@' ).
    <fs_out>-st_proc = '04'.

    TRY .
        zcl_faturamento=>zif_faturamento~get_instance(
          )->get_processo_emissao_docs(
            EXPORTING
              i_tknum       = CONV #( <fs_out>-transp )
            IMPORTING
              e_conhecimento = DATA(e_conhecimento)
              e_manifesto    = DATA(e_manifesto)
          ).

      CATCH zcx_faturamento INTO DATA(ex_faturamento).
      CATCH zcx_error INTO DATA(ex_error).
    ENDTRY.

  ENDIF.

  "Verificar se Nota Eletrônica """"""""""""""""""""""""""""""""""""""""""""""""""
  IF e_manifesto EQ abap_true.
    SELECT * INTO TABLE @DATA(it_zlest0110)
      FROM zlest0110
     WHERE vbeln EQ @<fs_out>-vbeln
       AND chave NE @space.

    IF sy-subrc IS NOT INITIAL.
      e_manifesto = abap_false.
    ENDIF.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


  IF ( <fs_out>-doccus IS NOT INITIAL ) AND ( <fs_out>-doccus(1) NE '@' ).
    <fs_out>-st_proc = '05'.
  ENDIF.

  IF e_conhecimento EQ abap_false AND e_manifesto EQ abap_true.

    "Emite Somente MDF-e
    IF ( <fs_out>-damdfe IS NOT INITIAL ) AND ( <fs_out>-damdfe(1) NE '@' ).
      <fs_out>-st_proc = '99'.
    ENDIF.

  ELSE.

    "Emite MDF-e pela CT-e
    IF ( <fs_out>-ovserv IS NOT INITIAL ) AND ( <fs_out>-ovserv(1) NE '@' ).
      <fs_out>-st_proc = '06'.
    ENDIF.

    IF ( <fs_out>-fatserv IS NOT INITIAL ) AND ( <fs_out>-fatserv(1) NE '@' ).
      <fs_out>-st_proc = '07'.
    ENDIF.

    IF ( <fs_out>-dacte IS NOT INITIAL ) AND ( <fs_out>-dacte(1) NE '@' ).
      <fs_out>-st_proc = '99'.
    ENDIF.

  ENDIF.

  UPDATE zlest0108 SET st_proc = <fs_out>-st_proc
   WHERE ebeln = <fs_out>-ebeln
     AND vbeln = <fs_out>-vbeln.

  CALL METHOD wa_alv_aviso_rec->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.

  CASE sy-ucomm.
    WHEN 'GERARPEDI'.
      CLEAR: sy-ucomm.
      CALL FUNCTION 'ZMF_PEDIDO_COMPRA_CRIAR'.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CARREGA_DADOS_ATRIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_dados_atrib .

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD wa_alv_ped->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  IF lines( it_sel_rows ) NE 1.
    MESSAGE 'Selecione apenas um item!' TYPE  'S'.
    EXIT.
  ENDIF.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    CLEAR: wa_saida.
    READ TABLE it_saida INTO wa_saida INDEX wa_sel_rows-index.

    PERFORM preenche_dados_atrib USING wa_saida.
  ENDLOOP.

  PERFORM abrir_aviso_rec.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_DADOS_ATRIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM preenche_dados_atrib  USING  p_wa_saida TYPE ty_saida.

  DATA: vl_idx_placa  TYPE lvc_index,
        vl_centro_in  TYPE werks_d,
        vl_centro_out TYPE werks_d.

  IF vg_view_transp IS NOT INITIAL.

    REFRESH: it_sel_rows, it_zlest0109.

    CALL METHOD wa_alv_aviso_rec->get_selected_rows
      IMPORTING
        et_index_rows = it_sel_rows.

    CHECK NOT it_sel_rows IS INITIAL.

    IF ( lines( it_sel_rows ) NE 1 ).
      MESSAGE 'Selecione uma linha para visualização' TYPE 'I'.
      EXIT.
    ENDIF.

    READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

    READ TABLE it_saida_aviso INTO wa_saida_aviso INDEX wa_sel_rows-index.

    SELECT *
      INTO TABLE it_zlest0109
      FROM zlest0109
     WHERE ebeln = wa_saida_aviso-ebeln
       AND vbeln = wa_saida_aviso-vbeln.

    LOOP AT it_zlest0109 INTO wa_zlest0109.

      CLEAR: wa_dados_aviso.

      MOVE-CORRESPONDING wa_zlest0109 TO wa_dados_aviso.

      SELECT SINGLE *
        INTO wa_mara
        FROM mara
       WHERE matnr = wa_zlest0109-matnr.

      wa_dados_aviso-unidade = wa_mara-meins.

      APPEND wa_dados_aviso TO it_dados_aviso.

    ENDLOOP.

    SELECT SINGLE *
      INTO wa_dados_transp
      FROM zlest0108
     WHERE ebeln = wa_saida_aviso-ebeln
       AND vbeln = wa_saida_aviso-vbeln.

    IF wa_dados_transp-placa_cav IS NOT INITIAL.
      wa_dados_veic-pc_veiculo = wa_dados_transp-placa_cav.
      APPEND wa_dados_veic TO it_dados_veic.
    ENDIF.

    IF wa_dados_transp-placa_car1 IS NOT INITIAL.
      wa_dados_veic-pc_veiculo = wa_dados_transp-placa_car1.
      APPEND wa_dados_veic TO it_dados_veic.
    ENDIF.

    vl_idx_placa = 1.
    LOOP AT it_dados_veic INTO wa_dados_veic.
      PERFORM atualiza_dados_placa USING wa_dados_veic  vl_idx_placa.
      ADD 1 TO  vl_idx_placa.
    ENDLOOP.

    IF wa_dados_transp-placa_car2 IS NOT INITIAL.
      wa_dados_veic-pc_veiculo = wa_dados_transp-placa_car2.
      APPEND wa_dados_veic TO it_dados_veic.
    ENDIF.

    IF wa_dados_transp-placa_car3 IS NOT INITIAL.
      wa_dados_veic-pc_veiculo = wa_dados_transp-placa_car3.
      APPEND wa_dados_veic TO it_dados_veic.
    ENDIF.

    SELECT SINGLE *
      INTO wa_dados_nf
      FROM zlest0110
     WHERE ebeln = wa_saida_aviso-ebeln
       AND vbeln = wa_saida_aviso-vbeln.


  ELSE.


    "------------------------------------------------------------------------------"
    "--\ Itens Aviso
    "------------------------------------------------------------------------------"
    CLEAR: wa_dados_aviso.

    "------------------------------------------------------------------------------"
    "Busca Aviso dados do aviso existente
    "------------------------------------------------------------------------------"
    REFRESH: it_buscar_aviso.
    DATA: w_alv_aviso LIKE LINE OF it_buscar_aviso.

    "Confirmações pedido
    SELECT ebeln, ebelp, etens, eindt, ezeit, menge, xblnr,
       vbeln, vbelp, ematn, charg  FROM ekes
        INTO TABLE @DATA(t_ekes)
         WHERE ebeln = @p_wa_saida-ebeln
           AND ebelp = @p_wa_saida-ebelp
           "AND ematn = @p_wa_saida-matnr
           AND xblnr = @p_wa_saida-xblnr.

    IF t_ekes[] IS NOT INITIAL.

      SORT t_ekes BY ebeln ebelp vbeln vbelp.
      DELETE ADJACENT DUPLICATES FROM t_ekes COMPARING ebeln ebelp vbeln vbelp.

      "Verifica se o emissor, gera NFe
*      SELECT lifnr, scacd  FROM lfa1
*        INTO TABLE @DATA(t_lfa1)
**        FOR ALL ENTRIES IN @t_ekes
*         WHERE lifnr = @p_wa_saida-lifnr.
**           and SCACD = 9999.


      "Item de transporte
      SELECT  *  FROM vttp
        INTO TABLE @DATA(t_vttp)
        FOR ALL ENTRIES IN @t_ekes
         WHERE vbeln = @t_ekes-vbeln.

      "Documento SD: fornecimento: dados de item
      SELECT vbeln, posnr, matnr, werks, lgort,
        charg, lfimg, meins, brgew, gewei, vgbel, vgpos,
        kcmeng, kcbrgew FROM lips
              INTO TABLE @DATA(it_lips)
                  FOR ALL ENTRIES IN @t_ekes
                   WHERE vbeln = @t_ekes-vbeln
                     AND posnr = @t_ekes-vbelp.

    ENDIF.

    IF it_lips[] IS NOT INITIAL.

      SELECT vbeln, lifnr, lifex FROM likp
        INTO TABLE @DATA(it_likp)
        FOR ALL ENTRIES IN @it_lips
        WHERE vbeln = @it_lips-vbeln.
    ENDIF.

    IF it_lips[] IS NOT INITIAL.

      DATA: r_parvw TYPE RANGE OF vbpa-parvw.

      r_parvw = VALUE #( ( sign = 'I' option = 'EQ' low = 'LR' )
      ( sign = 'I' option = 'EQ' low = 'PC' )
      ( sign = 'I' option = 'EQ' low = 'LF' )
                       ).
      SELECT vbeln, posnr, parvw, lifnr, kunnr FROM vbpa
        INTO TABLE @DATA(it_vbpa)
        FOR ALL ENTRIES IN @it_lips
        WHERE vbeln = @it_lips-vbeln
          AND parvw IN @r_parvw.

    ENDIF.


    IF t_vttp[] IS NOT INITIAL.

      "Cabeçalho transporte
      SELECT tknum, shtyp FROM vttk
        INTO TABLE @DATA(t_vttk)
        FOR ALL ENTRIES IN @t_vttp
        WHERE tknum = @t_vttp-tknum.

    ENDIF.

    IF t_vttk[] IS NOT INITIAL.

      "Tipos de transporte
      SELECT shtyp, vsart FROM tvtk
       INTO TABLE @DATA(t_tvtk)
        FOR ALL ENTRIES IN @t_vttk
        WHERE shtyp = @t_vttk-shtyp
         AND  vsart <> '01'.

    ENDIF.

    REFRESH: it_buscar_aviso.

    "Confirmações pedido
    LOOP AT t_ekes INTO DATA(w_ekes).

      READ TABLE it_likp INTO DATA(w_likp) WITH KEY vbeln = w_ekes-vbeln.

      CHECK sy-subrc IS INITIAL.

      w_alv_aviso-lifnr = w_likp-lifnr.
      w_alv_aviso-lifex = w_likp-lifex.

      READ TABLE it_lips INTO DATA(w_lips) WITH KEY vbeln = w_likp-vbeln
                                                   posnr = w_ekes-vbelp.
      CHECK sy-subrc IS INITIAL.

      w_alv_aviso-ebeln = w_ekes-ebeln."    Nr. Pedido
      w_alv_aviso-ebelp = w_ekes-ebelp."Item Pedido
      w_alv_aviso-vbeln = w_ekes-vbeln."Nr. Aviso

      w_alv_aviso-xblnr = w_ekes-xblnr. "Nota
      w_alv_aviso-eindt = w_ekes-eindt. "Data Aviso
      w_alv_aviso-ezeit = w_ekes-ezeit."Hora Aviso

      w_alv_aviso-matnr = w_lips-matnr."Material

      w_alv_aviso-werks = w_lips-werks.
      w_alv_aviso-lgort = w_lips-lgort.
      w_alv_aviso-charg = w_lips-charg.
      w_alv_aviso-gewei = w_lips-gewei.
      w_alv_aviso-vgbel = w_lips-vgbel.
      w_alv_aviso-vgpos = w_lips-vgpos.

*-IR 185421-03.07.2024-#144903-JT-inicio
      w_alv_aviso-lfimg      = w_lips-lfimg. "Qtdade
      w_alv_aviso-qtde_aviso = w_lips-brgew. "Peso Bruto

*     IF w_ekes-menge <> 0.
*       w_alv_aviso-lfimg = w_lips-lfimg. "Qtdade
*       w_alv_aviso-qtde_aviso = w_lips-brgew. "Peso Bruto
*     ENDIF.
*
*     IF w_ekes-menge = 0 AND w_ekes-charg  IS INITIAL.
*       w_alv_aviso-lfimg = w_lips-kcmeng. "Qtdade
*       w_alv_aviso-qtde_aviso = w_lips-kcbrgew. "Peso Bruto
*     ENDIF.
*-IR 185421-03.07.2024-#144903-JT-fim

      w_alv_aviso-unidade = w_lips-gewei. "Unidade

      READ TABLE it_vbpa INTO DATA(w_vbpa_fo) WITH KEY vbeln = w_lips-vbeln
                                                    parvw = 'LF'.
      IF sy-subrc IS INITIAL.
        w_alv_aviso-cod_remetente = w_vbpa_fo-lifnr.
      ENDIF.

      READ TABLE it_vbpa INTO DATA(w_vbpa_lr) WITH KEY vbeln = w_lips-vbeln
                                                    parvw = 'LR'.
      IF sy-subrc IS INITIAL.
        w_alv_aviso-cod_dest_merc = w_vbpa_lr-kunnr.
        w_alv_aviso-cod_loc_entrega = w_vbpa_lr-kunnr.
      ENDIF.

      READ TABLE it_vbpa INTO DATA(w_vbpa_pc) WITH KEY vbeln = w_lips-vbeln
                                                       parvw = 'PC'.
      IF sy-subrc IS INITIAL.
        w_alv_aviso-cod_loc_coleta = w_vbpa_pc-lifnr.
      ENDIF.

      READ TABLE t_vttp INTO DATA(w_vttp) WITH KEY vbeln = w_ekes-vbeln.
      IF  sy-subrc IS INITIAL.

        READ TABLE t_vttk INTO DATA(w_vttk) WITH KEY tknum = w_vttp-tknum.
        CHECK sy-subrc IS INITIAL.

        READ TABLE t_tvtk INTO DATA(w_tvtk) WITH KEY shtyp = w_vttk-shtyp.
        CHECK sy-subrc IS NOT INITIAL.


        APPEND w_alv_aviso TO it_buscar_aviso.

      ELSE.

        APPEND w_alv_aviso TO it_buscar_aviso.
      ENDIF.

      CLEAR: w_ekes, w_vttp, w_vttk, w_tvtk, w_alv_aviso, w_lips,
      w_vbpa_fo, w_vbpa_lr, w_vbpa_pc.

    ENDLOOP.
    "------------------------------------------------------------------------------"

    SELECT SINGLE *
      INTO wa_mara
      FROM mara
     WHERE matnr = p_wa_saida-matnr.

    wa_dados_aviso-ebelp   = p_wa_saida-ebelp.
    wa_dados_aviso-matnr   = p_wa_saida-matnr.
    wa_dados_aviso-ebeln   = p_wa_saida-ebeln.
    wa_dados_aviso-lifnr   = p_wa_saida-lifnr.
    wa_dados_aviso-unidade = wa_mara-meins.
    wa_dados_aviso-werks   = p_wa_saida-werks.
    wa_dados_aviso-lgort   = p_wa_saida-lgort.
    wa_dados_aviso-charg   = p_wa_saida-charg.

    "------------------------------------------------------------------------------"
    "Preenche dados do aviso existente
    "------------------------------------------------------------------------------"
    READ TABLE it_buscar_aviso INTO w_alv_aviso WITH KEY ebeln = p_wa_saida-ebeln
                                                         ebelp = p_wa_saida-ebelp
                                                         matnr = p_wa_saida-matnr
                                                         xblnr = p_wa_saida-xblnr.

    CHECK sy-subrc IS INITIAL.

    wa_dados_aviso-lifnr      = w_alv_aviso-lifnr.
    wa_dados_aviso-unidade    = w_alv_aviso-unidade.
    wa_dados_aviso-vbeln      = w_alv_aviso-vbeln.


    CLEAR: wa_dados_aviso-qtde_aviso.
    LOOP AT it_buscar_aviso INTO DATA(w_alv_aviso_aux) WHERE ebeln = p_wa_saida-ebeln
                                                         AND ebelp = p_wa_saida-ebelp
                                                         AND matnr = p_wa_saida-matnr
                                                         AND xblnr = p_wa_saida-xblnr.
      ADD w_alv_aviso_aux-qtde_aviso TO wa_dados_aviso-qtde_aviso.
    ENDLOOP.

    IF w_alv_aviso-werks IS NOT INITIAL.
      wa_dados_aviso-werks   = w_alv_aviso-werks.
    ENDIF.

    IF w_alv_aviso-lgort IS NOT INITIAL.
      wa_dados_aviso-lgort   = w_alv_aviso-lgort.
    ENDIF.

    IF w_alv_aviso-charg IS NOT INITIAL.
      wa_dados_aviso-charg   = w_alv_aviso-charg.
    ENDIF.

    APPEND wa_dados_aviso TO it_dados_aviso.

    "------------------------------------------------------------------------------"
    "--\ Parceiros
    "------------------------------------------------------------------------------"
    wa_dados_transp-cod_remetente = wa_saida-lifnr.

    CLEAR: vl_centro_out, vl_centro_in.

    vl_centro_in = p_wa_saida-werks.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro        = vl_centro_in
        tp_centro_out = 'R'
      IMPORTING
        centro_real   = vl_centro_out.

    IF ( sy-subrc EQ 0 ).

      wa_dados_transp-cod_dest_merc = vl_centro_out.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_dados_transp-cod_dest_merc
        IMPORTING
          output = wa_dados_transp-cod_dest_merc.

      wa_dados_transp-cod_loc_entrega = wa_dados_transp-cod_dest_merc.
    ENDIF.

    IF wa_dados_transp-cod_remetente IS INITIAL.
      wa_dados_transp-cod_remetente   = w_alv_aviso-cod_remetente.
    ENDIF.
    IF wa_dados_transp-cod_dest_merc IS INITIAL.
      wa_dados_transp-cod_dest_merc   = w_alv_aviso-cod_dest_merc.
    ENDIF.
    IF wa_dados_transp-cod_loc_coleta  IS INITIAL.
      wa_dados_transp-cod_loc_coleta  = w_alv_aviso-cod_loc_coleta.
    ENDIF.
    IF  wa_dados_transp-cod_loc_entrega IS INITIAL.
      wa_dados_transp-cod_loc_entrega = w_alv_aviso-cod_loc_entrega.
    ENDIF.

    IF wa_dados_transp-lgort IS INITIAL.
      wa_dados_transp-lgort = w_alv_aviso-lgort.
    ENDIF.

    "------------------------------------------------------------------------------"
    "--\ Dados NFe
    "------------------------------------------------------------------------------"
    SPLIT w_alv_aviso-xblnr AT '-' INTO wa_dados_nf-numero wa_dados_nf-serie.

    IF wa_dados_nf-numero IS NOT INITIAL AND wa_dados_transp-cod_remetente IS NOT INITIAL." Verifica se o emissor, gera NFe

      SELECT SINGLE lifnr, scacd  FROM lfa1
        INTO @DATA(wa_lfa1)
         WHERE lifnr = @wa_dados_transp-cod_remetente
           AND scacd = 9999.

      IF sy-subrc IS INITIAL.
        wa_dados_nf-modelo = '55'.
        CLEAR wa_lfa1.
      ENDIF.

    ENDIF.

    "------------------------------------------------------------------------------"
    "--\ Modal
    "------------------------------------------------------------------------------"
    IF strlen( wa_saida-charg ) = 4.
      wa_dados_transp-safra_ordem_car = wa_saida-charg.
    ENDIF.

    "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>
    PERFORM f_load_transp_carga_insumos.
    "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATRIBUIR_AVISO_RECEBIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atribuir_aviso_recebimento.

  DATA: var_answer      TYPE c,
        vl_idx_placas   TYPE i,
        vl_count_pc_cav TYPE i,
        vl_placa_cav    TYPE zlest0108-placa_cav,
        vmessa(80).

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row,
        vg_gerar_migo    TYPE char01,
        it_saida_sel     TYPE TABLE OF ty_saida WITH HEADER LINE,
        it_mov_estq      TYPE TABLE OF zmmt_ee_zgr,
        wa_mov_estq      TYPE zmmt_ee_zgr,
        it_mov_estq_ger  TYPE TABLE OF zmmt_ee_zgr_docs,
        wa_mov_estq_ger  TYPE zmmt_ee_zgr_docs,
        it_active_nfp    TYPE TABLE OF j_1bnfe_active,
        it_doc_nfp       TYPE TABLE OF j_1bnfdoc.

  DATA: vl_msg1       TYPE string,
        vl_msg2       TYPE string,
        vl_msg3       TYPE string,
        vl_msg4       TYPE string,
        vl_msg_exibir TYPE string,
        vl_nf_int     TYPE i,
        vl_serie_int  TYPE i,
        vl_nf_char    TYPE string,
        vl_serie_char TYPE string,
        vl_nf_serie   TYPE string,
        vl_quebra     TYPE string,
        vl_lifnr_dest TYPE lfa1-lifnr.


  IF vg_view_transp IS NOT INITIAL.
    MESSAGE 'Modo de visualização! Operação não permitida!' TYPE  'S'.
    EXIT.
  ENDIF.

  PERFORM preenche_dados_nf.

*  DATA(l_msg) =  'Confirma geração do Aviso de Recebimento?'.
*  IF r_atrib IS NOT INITIAL.
*    CLEAR l_msg.
*    l_msg =  'Confirma atribuição do Aviso de Receb.?'.
*  ENDIF.
*
*  CALL FUNCTION 'POPUP_TO_CONFIRM'
*    EXPORTING
*      titlebar              = 'Confirmação'
*      text_question         = l_msg "'Confirma geração do aviso de Recebimento?'
*      text_button_1         = 'Sim'
*      text_button_2         = 'Não'
*      default_button        = '1'
*      display_cancel_button = ''
*    IMPORTING
*      answer                = var_answer
*    EXCEPTIONS
*      text_not_found        = 1
*      OTHERS                = 2.
*
*  CHECK var_answer EQ '1'.

  REFRESH: it_zlest0109.
  CLEAR: wa_zlest0109, wa_dados_aviso.

  READ TABLE it_dados_aviso INTO wa_dados_aviso INDEX 1.

  CHECK wa_dados_aviso-vbeln IS NOT INITIAL.

  wa_dados_transp-ebeln = wa_dados_aviso-ebeln.
  wa_dados_transp-ebelp = wa_dados_aviso-ebelp.
  wa_dados_transp-werks = wa_dados_aviso-werks.
  wa_dados_transp-lgort = wa_dados_aviso-lgort.
  wa_dados_transp-charg = wa_dados_aviso-charg.

  wa_dados_nf-ebeln = wa_dados_aviso-ebeln.
  wa_dados_nf-ebelp = wa_dados_aviso-ebelp.
  wa_dados_nf-werks = wa_dados_aviso-werks.
  wa_dados_nf-lgort = wa_dados_aviso-lgort.
  wa_dados_nf-charg = wa_dados_aviso-charg.

  "-----------------------------------------------------------------------------
  "--- Conversion Exit Alpha Input
  "-----------------------------------------------------------------------------

  PERFORM conversion_exit_alpha.

  "-----------------------------------------------------------------------------
  "--- Validação Itens Aviso
  "-----------------------------------------------------------------------------

  LOOP AT it_dados_aviso INTO wa_dados_aviso.

    MOVE-CORRESPONDING wa_dados_aviso TO wa_zlest0109.
    wa_zlest0109-serie = wa_dados_nf-serie.
    wa_zlest0109-nfnum = wa_dados_nf-numero.

    IF ( wa_dados_aviso-qtde_aviso <= 0 ) OR ( wa_zlest0109-qtde_aviso <= 0 ).
      ROLLBACK WORK.
      MESSAGE 'Existe item(s) sem informar a quantidade do aviso!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_zlest0109-lifnr IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Fornecedor do pedido não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_zlest0109-nfnum IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Número da Nota não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_zlest0109-serie IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Série da Nota não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    APPEND wa_zlest0109 TO it_zlest0109.

  ENDLOOP.

  wa_dados_nf-lgort = wa_dados_transp-lgort.

  "-----------------------------------------------------------------------------
  "--- Validação Parceiros
  "-----------------------------------------------------------------------------
  IF wa_dados_transp-agente_frete IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Emissor do CT-e não informado!' TYPE 'S'.
    RETURN.
  ELSE.
    SELECT SINGLE *
      FROM lfa1
      INTO @DATA(wlfa1)
      WHERE lifnr = @wa_dados_transp-agente_frete.
    IF wlfa1-dlgrp NE '0001'.
      CONCATENATE 'Fornecedor' wa_dados_transp-agente_frete 'não configurado como agente de frete. Solicite ajuste à central de cadastro.' INTO  vmessa SEPARATED BY space.
      MESSAGE vmessa TYPE 'S'.
      RETURN.
    ENDIF.

    IF wlfa1-ktokk = 'ZFIC'.

      SELECT SINGLE * FROM marc
        INTO @DATA(w_marc)
        WHERE matnr = @wa_zlest0109-matnr
*          AND werks = @wlfa1-lifnr+6(4).
        AND werks = @wa_zlest0109-werks. "#IR17143 AOENNING

      IF sy-subrc  IS NOT INITIAL.
        MESSAGE s000(z_les) WITH 'Material ' wa_zlest0109-matnr ' não estendido para o centro' wlfa1-lifnr+6(4).
        RETURN.
      ENDIF.

    ENDIF.

    IF wlfa1-ktokk = 'ZFIC'.
      vg_tipo_frete = 'CIF'.
    ELSE.
      vg_tipo_frete = 'CPT'.
    ENDIF.
  ENDIF.

  IF wa_dados_transp-cod_remetente IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Remetente da Mercadoria não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_transp-cod_dest_merc IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Destinatário da Mercadoria não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_transp-cod_loc_coleta IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Local de Coleta não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_transp-cod_loc_entrega IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Local de Entrega não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  CLEAR: wa_lfa1, wa_kna1,  wa_trolz, wa_tvro.
  SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = wa_dados_transp-cod_loc_coleta.
  SELECT SINGLE * INTO wa_kna1 FROM kna1 WHERE kunnr = wa_dados_transp-cod_loc_entrega.

  SELECT SINGLE *
    INTO wa_trolz
    FROM trolz
   WHERE aland = 'BR'
     AND azone = wa_lfa1-lzone
     AND lland = 'BR'
     AND lzone = wa_kna1-lzone.

  IF wa_trolz-route IS INITIAL.
    ROLLBACK WORK.
    MESSAGE s000(zles) WITH 'Itinerário não encontrado. Zona transp. Orig\Dest:' wa_lfa1-lzone '\' wa_kna1-lzone. " diTYPE 'S'.
    RETURN.
  ENDIF.

  SELECT SINGLE * INTO wa_tvro FROM tvro WHERE route = wa_trolz-route.

  IF ( wa_tvro-tdiix IS INITIAL ) OR ( wa_trolz-route IS INITIAL ).
    ROLLBACK WORK.
    MESSAGE s000(zles) WITH 'Itinerário ' wa_trolz-route 'sem relevância para transporte. ' 'Solicite regularização para à logística.'. " TYPE 'S'.
    RETURN.
  ENDIF.

  "-----------------------------------------------------------------------------
  "--- Validação Dados NF-e
  "-----------------------------------------------------------------------------

  IF wa_dados_nf-modelo IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Modelo da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-numero IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Número da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-serie IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Série da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-dtemissao IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Data Emissão da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-cfop IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'CFOP da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-vl_produtos IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Valor Produtos da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF wa_dados_nf-vl_nota_fiscal IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Valor da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

*  IF wa_dados_nf-tp_produto IS INITIAL.
*    ROLLBACK WORK.
*    MESSAGE 'Tipo do Produto da Nota Fiscal é um campo obrigatório!' TYPE 'S'.
*    RETURN.
*  ENDIF.

  IF wa_dados_nf-nfe IS NOT INITIAL.

    IF strlen( wa_dados_nf-chave ) <> 44 .
      ROLLBACK WORK.
      MESSAGE 'Chave da NF-e não possui 44 dígitos!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_dados_nf-entrad EQ abap_true. "Entrada propria

      CLEAR: it_active_nfp[], it_doc_nfp[].

      DATA(_achou_entrada_prop) = abap_false.

      SELECT *
        FROM j_1bnfe_active INTO TABLE it_active_nfp
       WHERE stcd1  EQ wa_dados_nf-stcd1
         AND nfnum9 EQ wa_dados_nf-numero
         AND serie  EQ wa_dados_nf-serie.

      IF it_active_nfp[] IS NOT INITIAL.
        SELECT *
          FROM j_1bnfdoc INTO TABLE it_doc_nfp
           FOR ALL ENTRIES IN it_active_nfp
         WHERE docnum  EQ it_active_nfp-docnum.

        LOOP AT it_doc_nfp INTO DATA(wl_doc) WHERE entrad EQ abap_true
                                               AND candat IS INITIAL.
          _achou_entrada_prop = abap_true.
          EXIT.
        ENDLOOP.
      ENDIF.

      IF _achou_entrada_prop EQ abap_false.
        ROLLBACK WORK.
        MESSAGE |Documento entrada própria não localizado com a chave: { wa_dados_nf-chave }! | TYPE 'S'.
        RETURN.
      ENDIF.

    ELSE.

      SELECT SINGLE *
        FROM zib_nfe_dist_ter INTO @DATA(wl_zib_nfe_dist_ter_tmp)
       WHERE chave_nfe EQ @wa_dados_nf-chave.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE |XML não recebido para a chave: { wa_dados_nf-chave }! | TYPE 'S'.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zib_nfe_dist_itm INTO @DATA(wl_zib_nfe_dist_itm_tmp)
       WHERE chave_nfe EQ @wa_dados_nf-chave.

      vl_lifnr_dest = |{ wa_dados_transp-cod_dest_merc ALPHA = IN }|.

      SELECT SINGLE *
        FROM lfa1 INTO @DATA(_wl_lfa1_dest)
       WHERE lifnr EQ @vl_lifnr_dest.

      IF ( sy-subrc EQ 0 ) AND ( vl_lifnr_dest IS NOT INITIAL ) AND ( _wl_lfa1_dest-ktokk = 'ZFIC' ).
        IF wl_zib_nfe_dist_ter_tmp-branch NE _wl_lfa1_dest-lifnr+6(4).
          ROLLBACK WORK.
          MESSAGE |Destinatario XML: { wl_zib_nfe_dist_ter_tmp-branch } diferente do informado: { _wl_lfa1_dest-lifnr+6(4) }! | TYPE 'S'.
          RETURN.
        ENDIF.
      ENDIF.

      IF wa_dados_nf-vl_bc NE wl_zib_nfe_dist_ter_tmp-vl_icms_base.
        ROLLBACK WORK.
        MESSAGE |Base de Calculo ICMS divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_icms NE wl_zib_nfe_dist_ter_tmp-vl_icms_total.
        ROLLBACK WORK.
        MESSAGE |Valor ICMS divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_bc_st NE wl_zib_nfe_dist_ter_tmp-vl_icms_st_base.
        ROLLBACK WORK.
        MESSAGE |Base Calculo ST divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_st NE wl_zib_nfe_dist_ter_tmp-vl_icms_st_total.
        ROLLBACK WORK.
        MESSAGE |Valor ICMS ST divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_produtos NE wl_zib_nfe_dist_ter_tmp-vl_total.
        ROLLBACK WORK.
        MESSAGE |Valor Produtos divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-vl_nota_fiscal NE wl_zib_nfe_dist_ter_tmp-vl_total.
        ROLLBACK WORK.
        MESSAGE |Valor Nota Fiscal divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      IF wa_dados_nf-dtemissao NE wl_zib_nfe_dist_ter_tmp-dt_emissao.
        ROLLBACK WORK.
        MESSAGE |Data Emissão divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.

      DATA(cfop_tmp) = wl_zib_nfe_dist_itm_tmp-prod_cfop && 'AA'.
      IF wa_dados_nf-cfop NE cfop_tmp.
        ROLLBACK WORK.
        MESSAGE |CFOP divergente do XML| TYPE 'S'.
        RETURN.
      ENDIF.


    ENDIF.

  ENDIF.

  CLEAR: wa_zlest0109, wa_dados_aviso.
  READ TABLE it_dados_aviso INTO wa_dados_aviso INDEX 1.

  SELECT SINGLE *
    INTO wa_zlest0109
    FROM zlest0109
   WHERE ebeln = wa_dados_aviso-ebeln
     AND nfnum = wa_dados_nf-numero
     AND serie = wa_dados_nf-serie.

  IF sy-subrc = 0.

    CLEAR: wa_likp.
    SELECT SINGLE *
      INTO wa_likp
      FROM likp
     WHERE vbeln =  wa_zlest0109-vbeln.

    IF sy-subrc = 0.

      ROLLBACK WORK.
      CONCATENATE 'Já existe um aviso de Recebimento(' wa_zlest0109-vbeln
                  ') criado para esse Pedido/NF/Série!'
             INTO vl_msg_exibir SEPARATED BY space.
      MESSAGE vl_msg_exibir TYPE 'S'.
      RETURN.
    ENDIF.

  ENDIF.

  "-----------------------------------------------------------------------------
  "--- Validação Modal
  "-----------------------------------------------------------------------------
  IF ( wa_dados_transp-safra_ordem_car IS INITIAL OR
       wa_dados_transp-nro_ordem_car   IS INITIAL  ) AND vg_tipo_frete = 'CIF'.
    ROLLBACK WORK.
    MESSAGE 'Safra da Ordem de Carregamento é um campo obrigatório!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF vg_tipo_frete EQ 'CIF'.

    SELECT SINGLE *
      INTO wa_zlest0108_aux
      FROM zlest0108
     WHERE safra_ordem_car = wa_dados_transp-safra_ordem_car
       AND nro_ordem_car   = wa_dados_transp-nro_ordem_car.

    IF sy-subrc = 0.

      CLEAR: wa_likp.

      SELECT SINGLE *
        INTO wa_likp
        FROM likp
       WHERE vbeln = wa_zlest0108_aux-vbeln.

      IF sy-subrc = 0.

        vl_msg1 = wa_dados_transp-safra_ordem_car.
        vl_msg2 = wa_dados_transp-nro_ordem_car.
        vl_msg3 = wa_zlest0108_aux-vbeln.

        CONCATENATE 'Ordem de Carregamento de número:' space vl_msg2
                    ', referente a Safra:' space vl_msg1
                    ', já foi utilizada no Aviso de Recebimento: ' vl_msg3
                    '!'
               INTO vl_msg_exibir.

        ROLLBACK WORK.
        MESSAGE vl_msg_exibir TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

  ENDIF.

  IF wa_dados_transp-motorista IS INITIAL  AND ( vg_tipo_frete EQ 'CIF' ).
    ROLLBACK WORK.
    MESSAGE 'Motorista não informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF it_dados_veic[] IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Nenhuma placa encontrada!' TYPE 'S'.
    RETURN.
  ENDIF.

  CLEAR: vl_placa_cav.
  vl_count_pc_cav = 0.
  vl_idx_placas   = 0.
  LOOP AT it_dados_veic INTO wa_dados_veic WHERE pc_veiculo IS NOT INITIAL.
    IF wa_dados_veic-tp_veiculo = '0'.
      ADD 1 TO vl_count_pc_cav.
      vl_placa_cav = wa_dados_veic-pc_veiculo.
    ENDIF.
    ADD 1 TO vl_idx_placas.
  ENDLOOP.

  IF vl_count_pc_cav > 1.
    ROLLBACK WORK.
    MESSAGE 'Determinado mais de um veiculo de tração! Operação não permitida!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF vl_placa_cav IS INITIAL.
    ROLLBACK WORK.
    MESSAGE 'Não informado o veiculo de tração! Operação não permitida!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF vl_idx_placas > 4.
    ROLLBACK WORK.
    MESSAGE 'Não é permitido informar mais que 4 placas!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF vl_idx_placas <= 0.
    ROLLBACK WORK.
    MESSAGE 'Informar pelo menos uma placa!' TYPE 'S'.
    EXIT.
  ENDIF.

  wa_dados_transp-placa_cav = vl_placa_cav.

  vl_idx_placas = 1.
  LOOP AT it_dados_veic INTO wa_dados_veic WHERE tp_veiculo <> '0'.

    CASE vl_idx_placas.
      WHEN 1.
        wa_dados_transp-placa_car1 = wa_dados_veic-pc_veiculo.
      WHEN 2.
        wa_dados_transp-placa_car2 = wa_dados_veic-pc_veiculo.
      WHEN 3.
        wa_dados_transp-placa_car3 = wa_dados_veic-pc_veiculo.
    ENDCASE.

    ADD 1 TO vl_idx_placas.

  ENDLOOP.

  "-----------------------------------------------------------------------------
  "  Verifica se já tem um Aviso de Recebimento criado para
  "  o Pedido via Interface com o numero NF e Série atual.
  "-----------------------------------------------------------------------------
  CLEAR: wa_mov_estq_ger, wa_mov_estq  , vl_nf_int, vl_serie_int,
         vl_nf_char     , vl_serie_char, vl_nf_serie.

  REFRESH: it_mov_estq_ger, it_mov_estq.

  vl_nf_int      = wa_dados_nf-numero.
  vl_serie_int   = wa_dados_nf-serie.

  vl_nf_char    = vl_nf_int.
  vl_serie_char = vl_serie_int.
  CONCATENATE vl_nf_char '-' vl_serie_char INTO vl_nf_serie.
  CONDENSE vl_nf_serie NO-GAPS.

  IF ( vl_nf_char    IS INITIAL ) OR
     ( vl_serie_char IS INITIAL ) OR
     ( vl_nf_serie   IS INITIAL ) OR
     ( strlen( vl_nf_serie )  < 3 ).
    ROLLBACK WORK.
    MESSAGE 'Houve um erro ao validar a numeração da NF/Série!' TYPE 'S'.
    EXIT.
  ENDIF.

**============================================Comentado / aoenning /
*  CLEAR: it_mov_estq[].
*  SELECT *
*    FROM zmmt_ee_zgr INTO TABLE it_mov_estq
*   WHERE po_number      EQ wa_dados_aviso-ebeln
*     AND ref_doc_no     EQ vl_nf_serie
*     AND in_aviso_receb EQ 'S'.

*  LOOP AT it_mov_estq INTO wa_mov_estq.
*    CLEAR: wa_mov_estq_ger, wa_likp.
*
*    SELECT SINGLE *
*      FROM zmmt_ee_zgr_docs INTO wa_mov_estq_ger
*     WHERE obj_key EQ wa_mov_estq-obj_key.

*    IF ( sy-subrc = 0 ) AND ( wa_mov_estq_ger-av_vbeln IS NOT INITIAL ).

*      CLEAR: wa_likp.
*      SELECT SINGLE *
*        FROM likp INTO wa_likp
*       WHERE vbeln     EQ wa_mov_estq_ger-av_vbeln
*         AND spe_loekz EQ ''.

*      IF sy-subrc = 0.
*        vl_msg2   = vl_nf_serie.
*        vl_msg3   = wa_dados_aviso-ebeln.
*        vl_msg4   = wa_mov_estq-obj_key.
*
*        CONCATENATE 'Já existe um processo em andamento para criação de Aviso. Rec. c/ os dados seguir: '
*                    'Pedido:'      vl_msg3 '/'
*                    'NF-Série:'    vl_msg2 '/'
*                    'Chave Ref.:'  vl_msg4
*               INTO vl_msg_exibir SEPARATED BY space.
*
*        ROLLBACK WORK.
*        MESSAGE vl_msg_exibir TYPE 'S'.
*        RETURN.
*      ENDIF.

*    ENDIF.
*  ENDLOOP.
**============================================Comentado / aoenning /

  CLEAR: wa_mov_estq_ger, wa_mov_estq  , vl_nf_int, vl_serie_int,
         vl_nf_char     , vl_serie_char, vl_nf_serie.

  REFRESH: it_mov_estq_ger, it_mov_estq.


*  SELECT SINGLE * INTO @DATA(wa_ekko)
*    FROM ekko
*   WHERE ebeln EQ @wa_dados_transp-ebeln.

*  TRY .
*      IF wa_dados_nf-tp_produto IS NOT INITIAL.
*        zcl_deposito=>zif_deposito~get_instance(
*          )->get_deposito_material_filial(
*          EXPORTING
*            i_matnr          = wa_dados_nf-material    " Nº do material
*            i_tp_produto     = wa_dados_nf-tp_produto    " Tipo de Produto
*            i_bukrs          = wa_ekko-bukrs " Empresa
*            i_centro_a_fixar = wa_dados_transp-werks
*          IMPORTING
*            e_lgort          = wa_dados_transp-lgort ).
*      ELSE.
*        wa_dados_transp-lgort = 'ARMZ'.
*      ENDIF.
*    CATCH zcx_deposito.    "
*      wa_dados_transp-lgort = 'ARMZ'.
*  ENDTRY.

*  LOOP AT it_zlest0109 ASSIGNING FIELD-SYMBOL(<fs_0109>).
*    <fs_0109>-lgort = wa_dados_transp-lgort.
*  ENDLOOP.

*  wa_dados_nf-lgort = wa_dados_transp-lgort.

  CLEAR: wa_mov_estq_ger.
*  CALL FUNCTION 'Z_MM_CRIAR_AVISO'
*    EXPORTING
*      wa_zlest0108       = wa_dados_transp
*      wa_zlest0110       = wa_dados_nf
*      data_aviso         = sy-datum
*      gerar_apenas_aviso = 'X'
*      frete_ent_terc     = 'X'
*    IMPORTING
*      doc_gerados        = wa_mov_estq_ger
*    TABLES
*      it_zlest0109       = it_zlest0109
*    EXCEPTIONS
*      error              = 1
*      OTHERS             = 2.

  DATA(l_msg) =  'Confirma geração do Aviso de Recebimento?'.
  IF r_atrib IS NOT INITIAL.
    CLEAR l_msg.
    l_msg =  'Confirma atribuição do Aviso de Receb.?'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = l_msg "'Confirma geração do aviso de Recebimento?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.
  "-------------------------------------------------------------------
*  Número do Aviso
  "-------------------------------------------------------------------
  wa_mov_estq_ger-av_vbeln = wa_dados_aviso-vbeln.

  wa_dados_transp-vbeln = wa_mov_estq_ger-av_vbeln.
  MOVE-CORRESPONDING wa_dados_transp TO wa_zlest0108.
  MODIFY zlest0108 FROM   wa_zlest0108.

  wa_dados_nf-vbeln = wa_mov_estq_ger-av_vbeln.
  MOVE-CORRESPONDING wa_dados_nf TO wa_zlest0110.
  MODIFY zlest0110 FROM wa_zlest0110.

  LOOP AT it_zlest0109 ASSIGNING FIELD-SYMBOL(<fs_zlest0109>).
    <fs_zlest0109>-vbeln  = wa_mov_estq_ger-av_vbeln.
    MODIFY zlest0109 FROM <fs_zlest0109>.
  ENDLOOP.

  COMMIT WORK.

  IF sy-subrc IS INITIAL.
    MESSAGE s000(z_mm) WITH 'Recebimento' wa_mov_estq_ger-av_vbeln ' atribuido com sucesso!'.
  ELSE.
    MESSAGE e000(z_mm) WITH 'Erro ao tentar atribuir o' 'Aviso de recebimento!'.
    EXIT.
  ENDIF.

  PERFORM atualiza_aviso USING wa_saida. "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>

  IF wa_mov_estq_ger IS NOT INITIAL.
    LEAVE TO SCREEN 0.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_REL_PEDIDO_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_buscar_rel_pedido_nota .

  REFRESH: it_buscar_aviso.
  DATA: w_alv_aviso LIKE LINE OF it_buscar_aviso.

  "Confirmações pedido
  IF it_eket[] IS NOT INITIAL.

    SELECT ebeln, ebelp, etens, eindt, ezeit, menge, xblnr,
       vbeln, vbelp, ematn, charg  FROM ekes
        INTO TABLE @DATA(t_ekes)
      FOR ALL ENTRIES IN @it_eket
         WHERE ebeln = @it_eket-ebeln
           AND ebelp = @it_eket-ebelp.

    LOOP AT it_ekpo INTO DATA(ls_ekpo).
      LOOP AT t_ekes ASSIGNING FIELD-SYMBOL(<f_ekes>) WHERE ebeln EQ ls_ekpo-ebeln AND ebelp EQ ls_ekpo-ebelp.
        <f_ekes>-ematn = ls_ekpo-matnr.
      ENDLOOP.
    ENDLOOP.

    SORT t_ekes BY  ebeln ebelp xblnr vbeln vbelp.
    DELETE ADJACENT DUPLICATES FROM t_ekes COMPARING ebeln ebelp xblnr.

  ENDIF.

  IF t_ekes[] IS NOT INITIAL.

    SORT t_ekes BY ebeln ebelp vbeln vbelp.
    DELETE ADJACENT DUPLICATES FROM t_ekes COMPARING ebeln ebelp vbeln vbelp.

    "Item de transporte
    SELECT  *  FROM vttp
      INTO TABLE @DATA(t_vttp)
      FOR ALL ENTRIES IN @t_ekes
       WHERE vbeln = @t_ekes-vbeln.

    "Documento SD: fornecimento: dados de item
    SELECT vbeln, posnr, matnr, werks, lgort,
      charg, lfimg, meins, brgew, gewei, vgbel, vgpos,
      kcmeng, kcbrgew FROM lips
            INTO TABLE @DATA(t_lips)
                FOR ALL ENTRIES IN @t_ekes
                 WHERE vbeln = @t_ekes-vbeln
                   AND posnr = @t_ekes-vbelp
                   AND matnr = @t_ekes-ematn.

  ENDIF.

  IF t_lips[] IS NOT INITIAL.

    SELECT vbeln, lifnr, lifex FROM likp
      INTO TABLE @DATA(t_likp)
      FOR ALL ENTRIES IN @t_lips
      WHERE vbeln = @t_lips-vbeln.

  ENDIF.

*          IF it_lips[] IS NOT INITIAL.
*
*            DATA: r_parvw TYPE RANGE OF vbpa-parvw.
*
*            r_parvw = VALUE #( ( sign = 'I' option = 'EQ' low = 'LR' )
*            ( sign = 'I' option = 'EQ' low = 'PC' )
*            ( sign = 'I' option = 'EQ' low = 'LF' )
*                             ).
*            SELECT vbeln, posnr, parvw, lifnr, kunnr FROM vbpa
*              INTO TABLE @DATA(it_vbpa)
*              FOR ALL ENTRIES IN @it_lips
*              WHERE vbeln = @it_lips-vbeln
*                AND parvw IN @r_parvw.
*
*            ENDIF.

  IF t_vttp[] IS NOT INITIAL.

    "Cabeçalho transporte
    SELECT tknum, shtyp FROM vttk
      INTO TABLE @DATA(t_vttk)
      FOR ALL ENTRIES IN @t_vttp
      WHERE tknum = @t_vttp-tknum.

  ENDIF.

  IF t_vttk[] IS NOT INITIAL.

    "Tipos de transporte
    SELECT shtyp, vsart FROM tvtk
     INTO TABLE @DATA(t_tvtk)
      FOR ALL ENTRIES IN @t_vttk
      WHERE shtyp = @t_vttk-shtyp
       AND  vsart <> '01'.

  ENDIF.

  REFRESH: it_buscar_aviso.

  "Confirmações pedido
  LOOP AT t_ekes INTO DATA(w_ekes).

    READ TABLE t_likp INTO DATA(w_likp) WITH KEY vbeln = w_ekes-vbeln.

    CHECK sy-subrc IS INITIAL.

    w_alv_aviso-lifnr = w_likp-lifnr.
    w_alv_aviso-lifex = w_likp-lifex.

    READ TABLE t_lips INTO DATA(w_lips) WITH KEY vbeln = w_likp-vbeln
                                                 posnr = w_ekes-vbelp.
    CHECK sy-subrc IS INITIAL.

    w_alv_aviso-ebeln = w_ekes-ebeln."    Nr. Pedido
    w_alv_aviso-ebelp = w_ekes-ebelp."Item Pedido
    w_alv_aviso-vbeln = w_ekes-vbeln."Nr. Aviso

    w_alv_aviso-xblnr = w_ekes-xblnr. "Nota
    w_alv_aviso-eindt = w_ekes-eindt. "Data Aviso
    w_alv_aviso-ezeit = w_ekes-ezeit."Hora Aviso

    w_alv_aviso-matnr = w_lips-matnr."Material

    w_alv_aviso-werks = w_lips-werks.
    w_alv_aviso-lgort = w_lips-lgort.
    w_alv_aviso-charg = w_lips-charg.
    w_alv_aviso-gewei = w_lips-gewei.
    w_alv_aviso-vgbel = w_lips-vgbel.
    w_alv_aviso-vgpos = w_lips-vgpos.

    IF w_ekes-menge <> 0.
      w_alv_aviso-lfimg = w_lips-lfimg. "Qtdade
      w_alv_aviso-qtde_aviso = w_lips-brgew. "Peso Bruto
    ENDIF.

    IF w_ekes-menge = 0 AND w_ekes-charg  IS INITIAL.
      w_alv_aviso-lfimg = w_lips-kcmeng. "Qtdade
      w_alv_aviso-qtde_aviso = w_lips-kcbrgew. "Peso Bruto
    ENDIF.

    w_alv_aviso-unidade = w_lips-gewei.  "Unidade

    READ TABLE t_vttp INTO DATA(w_vttp) WITH KEY vbeln = w_ekes-vbeln.
    IF  sy-subrc IS INITIAL.

      READ TABLE t_vttk INTO DATA(w_vttk) WITH KEY tknum = w_vttp-tknum.
      CHECK sy-subrc IS INITIAL.

      READ TABLE t_tvtk INTO DATA(w_tvtk) WITH KEY shtyp = w_vttk-shtyp.
      CHECK sy-subrc IS NOT INITIAL.


      APPEND w_alv_aviso TO it_buscar_aviso.

    ELSE.

      APPEND w_alv_aviso TO it_buscar_aviso.
    ENDIF.

    CLEAR: w_ekes, w_vttp, w_vttk, w_tvtk, w_alv_aviso, w_lips.
*              w_vbpa_fo, w_vbpa_lr, w_vbpa_pc.

  ENDLOOP.

  REFRESH: t_ekes, t_vttp, t_vttk, t_tvtk, t_likp, t_lips.

ENDFORM.


FORM f_informa_transgenia USING p_ebeln TYPE ekko-ebeln
                       CHANGING c_informa TYPE c.

  c_informa = abap_true.

  SELECT SINGLE *
  FROM ekpo INTO @DATA(lwa_ekpo)
 WHERE ebeln EQ @p_ebeln.

  CHECK sy-subrc EQ 0 AND lwa_ekpo-matkl IS NOT INITIAL.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarvc)
   WHERE name = 'MAGGI_GR_FERTILIZANTES'
     AND low  = @lwa_ekpo-matkl.

  IF sy-subrc EQ 0 .
    CLEAR: c_informa.
    RETURN.
  ENDIF.

ENDFORM.

*-IR 185421-03.07.2024-#144903-JT-inicio
***********************************************************************
* marcar linhas automaticamente no grid
***********************************************************************
FORM f_marcar_linhas.

  DATA: l_tabix TYPE sy-tabix.

  CHECK it_xblnr[] IS NOT INITIAL.

  LOOP AT it_saida INTO wa_saida.
    l_tabix = sy-tabix.

    IF wa_saida-xblnr IN it_xblnr[].
      READ TABLE it_sel_rows INTO wa_sel_rows WITH KEY index = l_tabix.
      IF sy-subrc <> 0.
        wa_sel_rows-index   = l_tabix.
        APPEND wa_sel_rows TO it_sel_rows.
      ENDIF.
      EXIT.
    ENDIF.
  ENDLOOP.

  CALL METHOD wa_alv_ped->set_selected_rows
    EXPORTING
      it_index_rows = it_sel_rows.

ENDFORM.
*-IR 185421-03.07.2024-#144903-JT-fim
***********************************************************************
***********************************************************************

FORM f_load_transp_carga_insumos .

  DATA: vl_index TYPE lvc_index.

  CHECK r_atrib IS NOT INITIAL. "Opção de "Atribuição de Aviso"

  CHECK ( wa_dados_transp-safra_ordem_car  IS INITIAL ) AND
        ( wa_dados_transp-nro_ordem_car    IS INITIAL ) AND
        ( it_dados_veic[] IS INITIAL ) AND
        ( vg_view_transp IS INITIAL ).

  PERFORM: preenche_dados_nf,
           atualiza_campos_nf.

  CHECK ( strlen( wa_dados_nf-chave ) EQ 44 ).

  SELECT SINGLE *
    FROM zmmt0203 AS a INTO @DATA(lwa_zmmt0203)
   WHERE chave_nfe EQ @wa_dados_nf-chave
     AND EXISTS ( SELECT nro_cg
                    FROM zmmt0201 AS b
                    WHERE b~nro_cg = a~nro_cg
                      AND b~cancel = @abap_false ).

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
   WHERE nro_cg EQ @lwa_zmmt0203-nro_cg.

  CHECK sy-subrc EQ 0.

  CLEAR: wa_dados_transp-motorista.

  IF lwa_zmmt0201-placa_cav IS NOT INITIAL.
    APPEND INITIAL LINE TO it_dados_veic ASSIGNING FIELD-SYMBOL(<fs_dados_veiculo>).
    <fs_dados_veiculo>-pc_veiculo = lwa_zmmt0201-placa_cav.
    <fs_dados_veiculo>-tp_veiculo = '0'.
  ENDIF.

  IF lwa_zmmt0201-placa_car1 IS NOT INITIAL.
    APPEND INITIAL LINE TO it_dados_veic ASSIGNING <fs_dados_veiculo>.
    <fs_dados_veiculo>-pc_veiculo = lwa_zmmt0201-placa_car1.
    <fs_dados_veiculo>-tp_veiculo = '1'.
  ENDIF.

  IF lwa_zmmt0201-placa_car2 IS NOT INITIAL.
    APPEND INITIAL LINE TO it_dados_veic ASSIGNING <fs_dados_veiculo>.
    <fs_dados_veiculo>-pc_veiculo = lwa_zmmt0201-placa_car2.
    <fs_dados_veiculo>-tp_veiculo = '1'.
  ENDIF.

  IF lwa_zmmt0201-placa_car3 IS NOT INITIAL.
    APPEND INITIAL LINE TO it_dados_veic ASSIGNING <fs_dados_veiculo>.
    <fs_dados_veiculo>-pc_veiculo = lwa_zmmt0201-placa_car3.
    <fs_dados_veiculo>-tp_veiculo = '1'.
  ENDIF.

  wa_dados_transp-motorista    = lwa_zmmt0201-cod_motorista.
  wa_dados_transp-agente_frete = lwa_zmmt0201-cod_transportadora.
  wa_dados_transp-viagem_id    = lwa_zmmt0201-viagem_id.

  vl_index = 1.
  LOOP AT it_dados_veic INTO DATA(wa_dados_veic).
    PERFORM atualiza_dados_placa USING wa_dados_veic vl_index.
    ADD 1 TO vl_index.
  ENDLOOP.

  PERFORM atualiza_campos_parc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_remover_aviso_atribuido
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_remover_aviso_atribuido.

  DATA: var_answer TYPE c,
        wa_vbak    TYPE vbak,
        vl_vbeln   TYPE vbfa-vbeln,
        vl_mjahr   TYPE vbfa-mjahr,
        lva_tknum  TYPE vttk-tknum,
        vl_docnum  TYPE j_1bnflin-docnum.

  CALL METHOD wa_alv_aviso_rec->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK NOT it_sel_rows IS INITIAL.

  IF sy-tcode NE 'ZLES0113'.
    MESSAGE 'Transação apenas de visualização!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF ( lines( it_sel_rows ) NE 1 ).
    MESSAGE 'Selecione apenas uma linha!' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida_aviso ASSIGNING FIELD-SYMBOL(<fs_out>) INDEX wa_sel_rows-index.

  CHECK sy-subrc = 0.

  CHECK ( <fs_out>-ebeln IS NOT INITIAL ) AND ( <fs_out>-vbeln IS NOT INITIAL ).

  CLEAR: v_tknum.

  DATA(_delivery) = <fs_out>-vbeln.
  DATA(_vbtyp_v)  = '7'.

  IF _delivery IS INITIAL.
    MESSAGE 'Aviso não definido' TYPE 'I'.
    EXIT.
  ENDIF.

  IF ( <fs_out>-transp IS NOT INITIAL AND <fs_out>-transp(1) NE '@' ).
    MESSAGE 'Aviso com transporte gerado! Operação não permitida' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE vttk~tknum INTO lva_tknum
    FROM vbfa INNER JOIN vttk ON  vttk~tknum = vbfa~vbeln
                              AND vttk~vsart = '01'
   WHERE vbfa~vbelv    = _delivery
     AND vbfa~vbtyp_n  = '8'
     AND vbfa~vbtyp_v  = _vbtyp_v.

  IF sy-subrc = 0.
    MESSAGE 'Aviso com transporte gerado! Operação não permitida' TYPE 'I'.
    EXIT.
  ENDIF.

  DELETE FROM zlest0108 WHERE vbeln = _delivery.
  DELETE FROM zlest0109 WHERE vbeln = _delivery.
  DELETE FROM zlest0110 WHERE vbeln = _delivery.

  CALL METHOD wa_alv_aviso_rec->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.
