************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 28.08.2025                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Geração de Pagamento de Tributos - API              *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
REPORT zimp63.

TABLES: zimp_tipos_impos.
DATA:v_seq TYPE sy-tabix.
*-----------------------------------------------------------------------
* Tipos
*-----------------------------------------------------------------------
TYPES:
  BEGIN OF ty_j_1bbranch,
    bukrs      TYPE j_1bbranch-bukrs,
    branch     TYPE j_1bbranch-branch,
    state_insc TYPE j_1bbranch-state_insc,
    name       TYPE j_1bbranch-name,
  END OF ty_j_1bbranch,

  BEGIN OF ty_zimp_layout_imp,
    tp_imposto  TYPE zimp_layout_imp-tp_imposto,
    campolayout TYPE zimp_layout_imp-campolayout,
    camposabert TYPE zimp_layout_imp-camposabert,
  END OF ty_zimp_layout_imp,

  BEGIN OF ty_pagamento,
    valor_pagamento(15) TYPE c,
  END OF ty_pagamento.

*-----------------------------------------------------------------------
* Símbolos de campo
*-----------------------------------------------------------------------
FIELD-SYMBOLS:
  <f_zimp_cabecalho> TYPE zimp_lanc_impost,
  <f_zimp_detalhe>   TYPE zimp_lanc_imp_ct,
  <fs_campo>         TYPE any,
  <wa_data>          TYPE any.

*-----------------------------------------------------------------------
* Variáveis
*-----------------------------------------------------------------------
DATA:
      wa_bkpf              TYPE bkpf,
      t_zimp_cabecalho     TYPE TABLE OF zimp_lanc_impost,
      t_zimp_cabecalho_aux TYPE TABLE OF zimp_lanc_impost,
      t_zimp_layout_imp    TYPE TABLE OF ty_zimp_layout_imp,
      w_zimp_cabecalho     TYPE          zimp_lanc_impost,
      aux_zimp_contas_cons TYPE TABLE OF zimp_lanc_impost,
      t_zimp_detalhe       TYPE TABLE OF zimp_lanc_imp_ct,
      aux_zimp_detalhe     TYPE TABLE OF zimp_lanc_imp_ct,
      t_tvarvc             TYPE TABLE OF tvarvc,
      t_j_1bbranch         TYPE TABLE OF ty_j_1bbranch,
      wa_zib_contabil_chv  TYPE zib_contabil_chv,
      w_zimp_layout_imp    TYPE ty_zimp_layout_imp,
      w_pagamento          TYPE ty_pagamento.

DATA: vobj_key        TYPE zib_contabil_err-obj_key,
      codigo_banco(5) TYPE c,
      conv_banco      TYPE  t045t-dtaid,
      v_tit           TYPE zimp_lanc_imp_ct-valor_imp,
      vlifnr          TYPE zimp_lanc_imp_ct-lifnr.

DATA: v_codBanco(4).

*-----------------------------------------------------------------------
* Parâmetros de seleção
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-t01.
  PARAMETERS:
    p_bncemp                      TYPE t012-hbkid DEFAULT 'BBRA'.
  SELECT-OPTIONS:
    s_nr_doc                      FOR <f_zimp_detalhe>-doc_imposto,
    s_lifnr                       FOR <f_zimp_detalhe>-lifnr.
  PARAMETERS:
    p_emp                         TYPE b120-bukrs
                                  OBLIGATORY.
  SELECT-OPTIONS:
    s_dtlanc                      FOR wa_bkpf-budat,
    s_dtvenc                      FOR <f_zimp_cabecalho>-dt_venc OBLIGATORY,
    s_tparrc                      FOR zimp_tipos_impos-tp_arrec
                                  NO INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

AT SELECTION-SCREEN.
  IF NOT s_dtvenc IS INITIAL.
    LOOP AT s_dtvenc.
      IF s_dtvenc-low < sy-datum.
        MESSAGE i398(00) WITH 'Data de vencimento dos titulos'
                              'menor que data atual'.
        STOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tparrc-low.
  PERFORM match_code_tparrc.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM zf_carrega_tabelas.

  IF t_zimp_detalhe[] IS INITIAL OR t_zimp_cabecalho[] IS INITIAL.
    MESSAGE  'Não foram encontrados dados para os parâmetros informados.' TYPE 'I'.
    RETURN.
  ELSE.
    PERFORM zf_envia_dados_api.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form zf_carrega_tabelas
*&---------------------------------------------------------------------*
FORM zf_carrega_tabelas .

  SELECT  zimp_lanc_impost~mandt
          zimp_lanc_impost~doc_imposto
          zimp_lanc_impost~bukrs
          zimp_lanc_impost~lote
          zimp_lanc_impost~dt_venc
          zimp_lanc_impost~dt_apuracao
          zimp_lanc_impost~mes_apuracao
          zimp_lanc_impost~ano_apuracao
          zimp_lanc_impost~observacao
          zimp_lanc_impost~cod_imposto
          zimp_lanc_impost~ref_imposto
          zimp_lanc_impost~tp_imposto
          zimp_lanc_impost~cod_pgto
          zimp_lanc_impost~conv_banco
          zimp_lanc_impost~hbkid
          zimp_lanc_impost~gsber
          zimp_lanc_impost~waers
          zimp_lanc_impost~waers_f
          zimp_lanc_impost~identificador
          zimp_lanc_impost~cod_barras
          zimp_lanc_impost~qrcode
          zimp_lanc_impost~data_atual
          zimp_lanc_impost~hora_atual
          zimp_lanc_impost~usuario
          zimp_lanc_impost~loekz
*  Inicio Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779
          zimp_lanc_impost~moeda_gp_hist
          zimp_lanc_impost~st_fecha
          zimp_lanc_impost~kostl
          zimp_lanc_impost~zimp_lanc_impost
*  Fim Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779
    INTO TABLE t_zimp_cabecalho
      FROM zimp_lanc_impost
    INNER JOIN zimp_cad_lote
      ON zimp_cad_lote~lote = zimp_lanc_impost~lote
    AND zimp_cad_lote~status_lote = 'A'
      WHERE zimp_lanc_impost~bukrs       = p_emp     AND
            zimp_lanc_impost~doc_imposto IN s_nr_doc AND
            zimp_lanc_impost~dt_venc     IN s_dtvenc AND
            zimp_lanc_impost~tp_imposto  IN s_tparrc AND
            zimp_lanc_impost~hbkid  = p_bncemp AND
            zimp_lanc_impost~loekz  NE 'X'.


  SORT t_zimp_cabecalho BY doc_imposto.

  SELECT *
    INTO TABLE t_zimp_detalhe
  FROM zimp_lanc_imp_ct
    FOR ALL ENTRIES IN t_zimp_cabecalho
      WHERE doc_imposto = t_zimp_cabecalho-doc_imposto
      AND   bukrs       = t_zimp_cabecalho-bukrs.


  SELECT
    j_1bbranch~bukrs
    j_1bbranch~branch
    j_1bbranch~state_insc
    j_1bbranch~name
    FROM j_1bbranch
    INTO TABLE t_j_1bbranch
    FOR ALL ENTRIES IN t_zimp_cabecalho
  WHERE bukrs = t_zimp_cabecalho-bukrs
    AND ( branch = t_zimp_cabecalho-gsber OR branch = '0001' ).


  SELECT tp_imposto campolayout camposabert
    FROM zimp_layout_imp
   INTO TABLE t_zimp_layout_imp.

  IF NOT  s_lifnr IS INITIAL.
    LOOP AT t_zimp_detalhe ASSIGNING <f_zimp_detalhe>.
      IF <f_zimp_detalhe>-lifnr IS NOT INITIAL.
        IF <f_zimp_detalhe>-lifnr NOT IN s_lifnr.
          READ TABLE t_zimp_cabecalho INTO w_zimp_cabecalho WITH KEY doc_imposto = <f_zimp_detalhe>-doc_imposto BINARY SEARCH.
          IF sy-subrc = 0.
            w_zimp_cabecalho-loekz = 'X'.
            MODIFY t_zimp_cabecalho FROM w_zimp_cabecalho INDEX  sy-tabix TRANSPORTING loekz.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.

    IF s_tparrc-low = '08' AND <f_zimp_cabecalho>-ref_imposto = 'ZGL059' . "119896 AJUSTE TRANSAÇÃO PARA PAGAMENTOS DE CONSUMO - PSA
      wa_zib_contabil_chv-bukrs = <f_zimp_cabecalho>-bukrs.
      wa_zib_contabil_chv-belnr = <f_zimp_cabecalho>-doc_imposto.
      wa_zib_contabil_chv-gjahr = <f_zimp_cabecalho>-ano_apuracao.
    ELSE.
      CONCATENATE 'ZP' <f_zimp_cabecalho>-bukrs <f_zimp_cabecalho>-doc_imposto '%' INTO vobj_key.
      SELECT SINGLE *
        FROM zib_contabil_chv
      INTO wa_zib_contabil_chv
        WHERE obj_key LIKE vobj_key.
      IF sy-subrc NE 0.
        CONCATENATE 'ZIMP' <f_zimp_cabecalho>-doc_imposto '%' INTO vobj_key.
        SELECT SINGLE *
          FROM zib_contabil_chv
        INTO wa_zib_contabil_chv
          WHERE obj_key LIKE vobj_key
           AND  bukrs   =  <f_zimp_cabecalho>-bukrs .
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.
      SELECT SINGLE *
        FROM bkpf
      INTO wa_bkpf
        WHERE bukrs = wa_zib_contabil_chv-bukrs
          AND belnr = wa_zib_contabil_chv-belnr
          AND gjahr = wa_zib_contabil_chv-gjahr.
      IF sy-subrc = 0.
        IF s_dtlanc IS NOT INITIAL AND NOT wa_bkpf-budat IN s_dtlanc.
          MODIFY t_zimp_cabecalho FROM <f_zimp_cabecalho> INDEX sy-tabix TRANSPORTING loekz.
        ENDIF.
      ENDIF.
    ELSE.
      <f_zimp_cabecalho>-loekz = 'X'.
      MODIFY t_zimp_cabecalho FROM <f_zimp_cabecalho> INDEX  sy-tabix TRANSPORTING loekz.
    ENDIF.

    PERFORM zf_codigo_banco USING codigo_banco.

    SELECT SINGLE dtaid
      INTO conv_banco
    FROM  t045t
      WHERE zlsch = 'E'
        AND bukrs = <f_zimp_cabecalho>-bukrs
        AND hbkid = p_bncemp.
  ENDLOOP.

  DELETE t_zimp_cabecalho WHERE loekz = 'X'.
ENDFORM.                    " ZF_CARREGA_TABELAS

*&---------------------------------------------------------------------*
*& Form match_code_tparrc
*&---------------------------------------------------------------------*
FORM match_code_tparrc .

  DATA: BEGIN OF t_value OCCURS 0,
          line(30),
        END OF t_value,

        BEGIN OF t_zimp_tipos_impos OCCURS 0.
          INCLUDE STRUCTURE zimp_tipos_impos.
  DATA: END OF t_zimp_tipos_impos.

  DATA: t_hfields LIKE help_value OCCURS 0 WITH HEADER LINE.

  CLEAR t_hfields.
  t_hfields-tabname = 'ZIMP_TIPOS_IMPOS'.
  t_hfields-fieldname = 'TP_ARREC'.
  t_hfields-selectflag = 'X'.
  APPEND t_hfields.

  CLEAR t_hfields.
  t_hfields-tabname = 'ZIMP_TIPOS_IMPOS'.
  t_hfields-fieldname = 'ARRECADACAO'.
  APPEND t_hfields.

  SELECT *
    FROM zimp_tipos_impos
    WHERE tp_arrec IS NOT INITIAL
    INTO TABLE @t_zimp_tipos_impos.

  SORT t_zimp_tipos_impos BY tp_arrec.

  LOOP AT t_zimp_tipos_impos.

    t_value-line = t_zimp_tipos_impos-tp_arrec.
    APPEND t_value.

    t_value-line = t_zimp_tipos_impos-arrecadacao.
    APPEND t_value.

  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
    EXPORTING
      titel                     = TEXT-001
    IMPORTING
      select_value              = s_tparrc-low
    TABLES
      fields                    = t_hfields
      valuetab                  = t_value
    EXCEPTIONS
      field_not_in_ddic         = 1
      more_then_one_selectfield = 2
      no_selectfield            = 3
      OTHERS                    = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " MATCH_CODE_TPARRC


*&---------------------------------------------------------------------*
*& Form zf_envia_dados_api
*&---------------------------------------------------------------------*
FORM zf_envia_dados_api.

  DATA: lt_zimt004 TYPE TABLE OF zimt004,
        lw_zimt004 LIKE LINE  OF lt_zimt004.

  DATA: agencia(8) TYPE c,
        conta(9)   TYPE c,
        dv_agencia TYPE c,
        dv_conta   TYPE c.

  DATA: ls_in_bbd_pag_cont TYPE zde_in_bbd_pag_cont,
        lt_retorno         TYPE zde_in_bbd_pagcont_ret.


  DATA: lv_amount TYPE p DECIMALS 2 VALUE '123.45'.

  LOOP AT t_zimp_cabecalho INTO DATA(w_zimp).
    CONCATENATE w_zimp-cod_barras+0(3) '%' INTO v_codbanco.
    SELECT SINGLE * FROM bnka INTO @DATA(w_bnka_aux)
          WHERE bankl LIKE @v_codbanco.
    IF sy-subrc IS NOT INITIAL. "SE CODIGO BARRAS = COMECA COM BANCO SEGMENTO 'J' Se não é 'O'.
      APPEND w_zimp TO t_zimp_cabecalho_aux.
    ENDIF.
  ENDLOOP.

  IF t_zimp_cabecalho_aux[] IS NOT INITIAL.

    PERFORM zf_codigo_banco      USING codigo_banco.

    PERFORM zf_banco_agencia     USING agencia
                                       dv_agencia
                                       conta
                                       dv_conta.

    LOOP AT t_zimp_cabecalho_aux ASSIGNING <f_zimp_cabecalho>.

      ASSIGN w_pagamento TO <wa_data>.

      PERFORM preenche_valor.

      CONCATENATE <f_zimp_cabecalho>-dt_venc(4) '-' <f_zimp_cabecalho>-dt_venc+4(2) '-' <f_zimp_cabecalho>-dt_venc+6(2) INTO DATA(lv_dt_venc).

      ls_in_bbd_pag_cont =
          VALUE #(
                    agencia          = agencia
                    conta            = conta
                    digitoAgencia    = dv_agencia
                    digitoConta      = dv_conta
                    codigoBarras     = <f_zimp_cabecalho>-cod_barras
                    dataDebito       = lv_dt_venc
                    idTransacao      = <f_zimp_cabecalho>-doc_imposto+1(9)
                    tipoConta        = '1'
                    tipoRegistro     = '1'
                    valorPrincipal   = lv_amount "w_pagamento-valor_pagamento "é o valor_pagamento , zimp_layout_imp-camposabert
                    ).

      TRY.
          zcl_int_ob_fi_bradesco_pagcont=>zif_integracao_outbound~get_instance( )->execute_request(
            EXPORTING
              i_info_request         = ls_in_bbd_pag_cont
            IMPORTING
            e_id_integracao          = DATA(resul_id)
            e_integracao             = DATA(result_json)
          ).
          IF result_json-ds_data_retorno IS NOT INITIAL AND result_json-nm_code = '0200'.

            " Deserialize JSON with name mapping
            /ui2/cl_json=>deserialize(
              EXPORTING
                json          = result_json-ds_data_retorno
                name_mappings = VALUE #( ( json = 'campoChaveIdentificacaoDocumento' abap = 'CAMPOCHAVEIDENTIFICACAODOC' ) )
              CHANGING
                data          = lt_retorno
          ).

            lw_zimt004-mandt                           = sy-mandt.
            lw_zimt004-doc_imposto                     = <f_zimp_cabecalho>-doc_imposto.
            lw_zimt004-bukrs                           = <f_zimp_cabecalho>-bukrs.
            lw_zimt004-idtransacao                     = ls_in_bbd_pag_cont-idtransacao.
            lw_zimt004-retorno                         = lt_retorno-retorno. "Verificar

            PERFORM fm_retorno_desc USING lw_zimt004-retorno lw_zimt004-descricao .

            lw_zimt004-sql_code                         = result_json-nm_code.
            lw_zimt004-banco                            = lt_retorno-banco.
            lw_zimt004-agencia                          = lt_retorno-agencia.
            lw_zimt004-conta                            = lt_retorno-conta.
            lw_zimt004-tipoconta                        = lt_retorno-tipoconta.
            lw_zimt004-contadebito                      = lt_retorno-contadebito.
            lw_zimt004-nomecliente                      = lt_retorno-nomecliente.
            lw_zimt004-tiporegistro                     = lt_retorno-tiporegistro.
            lw_zimt004-autenticacaobancaria             = lt_retorno-autenticacaobancaria.

            REPLACE ALL OCCURRENCES OF '-' IN lt_retorno-datadebito WITH ''.
            CONDENSE lt_retorno-datadebito NO-GAPS.

            REPLACE ALL OCCURRENCES OF '-' IN lt_retorno-datavencimento WITH ''.
            CONDENSE lt_retorno-datavencimento NO-GAPS.

            lw_zimt004-datadebito                       = lt_retorno-datadebito.
            lw_zimt004-datavencimento                   = lt_retorno-datavencimento.
            lw_zimt004-valortributo                     = lt_retorno-valortributo.
            lw_zimt004-valorpago                        = lt_retorno-valorpago.
            lw_zimt004-valormulta                       = lt_retorno-valormulta.
            lw_zimt004-valorjurosmora                   = lt_retorno-valorjurosmora.
            lw_zimt004-valordesconto                    = lt_retorno-valordesconto.
            lw_zimt004-exercicio                        = lt_retorno-exercicio.
            lw_zimt004-codigotributo                    = lt_retorno-codigotributo.
            lw_zimt004-numeronr                         = lt_retorno-numeronr.
            lw_zimt004-identificacaocampochave          = lt_retorno-identificacaocampochave.
            lw_zimt004-campochaveidentificacaodocumen   = lt_retorno-campochaveidentificacaodoc.
            lw_zimt004-tipocomprovante                  = lt_retorno-tipocomprovante.
            lw_zimt004-cod_barras                       = lt_retorno-codigobarras.
            lw_zimt004-consulta_pgto                    = ''.
            lw_zimt004-dt_registro                      = sy-datum.
            lw_zimt004-hr_registro                      = sy-uzeit.
            lw_zimt004-us_registro                      = sy-uname.

            APPEND lw_zimt004 TO lt_zimt004.

            MODIFY zimt004 FROM TABLE lt_zimt004.
            COMMIT WORK AND WAIT.

          ENDIF.
        CATCH zcx_integracao INTO DATA(la_zcx_integracao).
        CATCH zcx_error INTO DATA(zcx_error).
      ENDTRY.

      CLEAR: lv_dt_venc ,w_pagamento,ls_in_bbd_pag_cont , lt_retorno, resul_id, result_json, lw_zimt004, lt_zimt004.

    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_codigo_banco
*&---------------------------------------------------------------------*
FORM zf_codigo_banco  USING p_codigo TYPE any.
  IF p_bncemp = 'BBRA'.
    p_codigo                    = '001'.
  ELSEIF p_bncemp = 'BBD'.
    p_codigo                    = '237'.
  ELSEIF p_bncemp(2) = 'IT'.
    p_codigo                    = '341'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_banco_agencia
*&---------------------------------------------------------------------*
FORM zf_banco_agencia  USING    p_agencia    TYPE any
                                p_dv_agencia TYPE any
                                p_conta      TYPE any
                                p_dv_conta   TYPE any.


  DATA: wl_agencia    TYPE bankk,
        wl_conta      TYPE bankn,
        wl_nome_banco TYPE bnka-banka,
        dv_agencia    TYPE bkont.


  DATA: lv_length    TYPE i,
        lv_last_four TYPE string,
        lv_offset(2) TYPE n,
        lv_conta     TYPE c LENGTH 18,
        lv_dig       TYPE c LENGTH 02.

  SELECT bankl UP TO 1 ROWS
    INTO wl_agencia
    FROM t012
    WHERE bukrs = p_emp AND
          hbkid = p_bncemp.
  ENDSELECT.

  SELECT bankn UP TO 1 ROWS
    INTO wl_conta
    FROM t012k
    WHERE bukrs = p_emp AND
          hbkid = p_bncemp.
  ENDSELECT.


*  SELECT banka UP TO 1 ROWS
*    INTO wl_nome_banco
*    FROM bnka
*    WHERE bankl = wl_agencia AND banks = 'BR'.
*  ENDSELECT.

  SELECT SINGLE dtgko
    INTO dv_agencia
    FROM t012d
    WHERE bukrs = p_emp AND
          hbkid = p_bncemp.


  lv_length = strlen( wl_agencia ).
  lv_offset = lv_length - 4.
  lv_last_four = wl_agencia+lv_offset(4).


  SPLIT wl_conta AT '-'
  INTO lv_conta  "Número da C/C
       lv_dig.   "Digito da Conta Corrente

  MOVE: lv_conta        TO p_conta,
        lv_dig          TO p_dv_conta,
        lv_last_four    TO p_agencia,
        "wl_nome_banco   TO p_nome_banco,
        dv_agencia      TO p_dv_agencia.

ENDFORM.                    " ZF_BANCO_AGENCIA
*&---------------------------------------------------------------------*
*& Form preenche_valor
*&---------------------------------------------------------------------*
FORM preenche_valor .

  DATA: string(50)    TYPE c,
        vcod_abertura TYPE zimp_lanc_imp_ct-cod_abertura,
        vcalculo      TYPE zimp_lanc_imp_ct-valor_imp,
        vtotal        TYPE zimp_lanc_imp_ct-valor_imp,
        xloop         TYPE i,
        xoperacao(1),
        vfield(50).

  CLEAR v_tit.

  LOOP AT t_zimp_layout_imp INTO w_zimp_layout_imp WHERE tp_imposto = <f_zimp_cabecalho>-tp_imposto.

    string = w_zimp_layout_imp-camposabert.
    vcalculo = 0.
    vtotal  = 0.

    CLEAR: vcod_abertura, xoperacao.

    WHILE string NE space.

      IF string(1) = '<'.
        CLEAR vcod_abertura.
      ELSEIF string(1) = '>'.
        ADD 1 TO xloop .
        vcalculo = 0.

        LOOP AT t_zimp_detalhe ASSIGNING <f_zimp_detalhe>
              WHERE doc_imposto  = <f_zimp_cabecalho>-doc_imposto
              AND   cod_abertura = vcod_abertura
              AND   bukrs        = <f_zimp_cabecalho>-bukrs.

          CASE vcod_abertura.
            WHEN '01'.
              vcalculo = <f_zimp_detalhe>-valor_imp.
            WHEN '07'.
              vcalculo = <f_zimp_detalhe>-valor_imp.
            WHEN '08'.
              vcalculo = <f_zimp_detalhe>-valor_imp.
            WHEN '11'.
              vcalculo = <f_zimp_detalhe>-valor_imp * -1.
            WHEN '17'.
              vcalculo = <f_zimp_detalhe>-valor_imp * -1.
            WHEN OTHERS.
*              VLIFNR = <F_ZIMP_DETALHE>-LIFNR.
          ENDCASE.

          IF <f_zimp_detalhe>-lifnr IS INITIAL.
            vcalculo = <f_zimp_detalhe>-valor_imp.
          ELSE.
            vlifnr = <f_zimp_detalhe>-lifnr.
          ENDIF.

        ENDLOOP.

        IF xoperacao  IS INITIAL.
          xoperacao = '+'.
        ENDIF.

        IF xoperacao = '+'.
          ADD vcalculo TO vtotal.
          CLEAR xoperacao.
        ELSEIF xoperacao = '-'.
          SUBTRACT vcalculo FROM vtotal.
          CLEAR xoperacao.
        ELSEIF xoperacao = '*'.
          vtotal = vtotal * vcalculo.
          CLEAR xoperacao.
        ELSEIF xoperacao = '/'.
          vtotal = vtotal / vcalculo.
          CLEAR xoperacao.
        ENDIF.

      ELSEIF '+_-_*_/' CS string(1) AND NOT string(1) IS INITIAL.
        xoperacao = string(1).
      ELSEIF NOT string(1) IS INITIAL.
        CONCATENATE vcod_abertura string(1) INTO vcod_abertura.
      ENDIF.
      SHIFT string.
    ENDWHILE.

    IF s_tparrc-low = '08' AND <f_zimp_detalhe>-vlr_moeda_doc IS NOT INITIAL. "119896 AJUSTE TRANSAÇÃO PARA PAGAMENTOS DE CONSUMO - PSA
      vtotal = <f_zimp_detalhe>-vlr_moeda_doc.
    ENDIF.

    IF vtotal GT 0.
      vfield = w_zimp_layout_imp-campolayout.
      CONDENSE vfield NO-GAPS.

      ASSIGN COMPONENT vfield  OF STRUCTURE <wa_data> TO <fs_campo>.

      IF sy-subrc = 0 AND vfield NE 'VALOR_TOTAL'.
        <fs_campo> = vtotal * 100.
      ENDIF.
      IF vcod_abertura = '11' OR vcod_abertura = '17' . "115457 GRU - CNAB - PSA -> adicionado GRU imposto é 14 mas a abertura é 17
        v_tit = v_tit + vtotal.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " PREENCHE_VALOR
*&---------------------------------------------------------------------*
*& Form fm_retorno_desc
*&---------------------------------------------------------------------*
FORM fm_retorno_desc  USING    p_retorno
                               p_descricao.

  CASE p_retorno.
    WHEN '000'. p_descricao =  'Ok'.
    WHEN '001'. p_descricao =  'Empresa não disponível para pagamento'                                                                             .
    WHEN '002'. p_descricao =  'Empresa não disponível para pagamento'                                                                             .
    WHEN '003'. p_descricao =  'Dígito do código de barras inválido'                                                                               .
    WHEN '004'. p_descricao =  'Opção de processamento inválida'                                                                                   .
    WHEN '006'. p_descricao =  'Conta já programada para débito'                                                                                   .
    WHEN '008'. p_descricao =  'Data de débito excede data-limite para pagamento'                                                                  .
    WHEN '009'. p_descricao =  'Data de débito inválida'                                                                                           .
    WHEN '010'. p_descricao =  'Agência / conta não cadastrada'                                                                                    .
    WHEN '011'. p_descricao =  'Código de barras inválido'                                                                                         .
    WHEN '012'. p_descricao =  'Data de débito excede data-limite para pagamento'                                                                  .
    WHEN '013'. p_descricao =  'Data de débito inválida'                                                                                           .
    WHEN '014'. p_descricao =  'Agência / conta inválida'                                                                                          .
    WHEN '015'. p_descricao =  'Saldo insuficiente'                                                                                                .
    WHEN '016'. p_descricao =  'Conta / competência bloqueada'                                                                                     .
    WHEN '017'. p_descricao =  'Tributo não autorizado para pagamento'                                                                             .
    WHEN '018'. p_descricao =  'Valor inconsistente'                                                                                               .
    WHEN '019'. p_descricao =  'Conta não autorizada para pagamento'                                                                               .
    WHEN '020'. p_descricao =  'Data do débito deve ser igual ao dia corrente'                                                                     .
    WHEN '022'. p_descricao =  'Documento não autorizado para pagamento'                                                                           .
    WHEN '023'. p_descricao =  'Data de vencimento inválida (não existe cadastramento para data do vencimento)'                                    .
    WHEN '024'. p_descricao =  'Problema no cálculo do valor (valor em moeda difere de real e não existe parametrização para a conversão)'         .
    WHEN '025'. p_descricao =  'Exercício não autorizado para pagamento'                                                                           .
    WHEN '027'. p_descricao =  'Anulação inválida (válido somente para os Canais Bradesco Expresso e Guichê de Caixa)'                             .
    WHEN '029'. p_descricao =  'Identificador digitado inválido (Tributo FGTS)'                                                                    .
    WHEN '030'. p_descricao =  'Valor de débito digitado inválido'                                                                                 .
    WHEN '031'. p_descricao =  'Conta com bloqueio judicial'                                                                                       .
    WHEN '034'. p_descricao =  'Data para efetivação do débito inválida (GPS)'                                                                     .
    WHEN '035'. p_descricao =  'Código do pagamento inválido (GPS)'                                                                                .
    WHEN '036'. p_descricao =  'Data de competência inválida (GPS)'                                                                                .
    WHEN '037'. p_descricao =  'Código do identificador do contribuinte inválido (GPS)'                                                            .
    WHEN '051'. p_descricao =  'Código de barras inconsistente - Tribunal de Justiça'                                                              .
    WHEN '052'. p_descricao =  'Pagamento via código de barras não autorizado (GPS)'                                                               .
    WHEN '053'. p_descricao =  'Competência 13 tem que ser paga dentro do próprio exercício (GPS)'                                                 .
    WHEN '054'. p_descricao =  'Canal desabilitado para pagamento'                                                                                 .
    WHEN '055'. p_descricao =  'Data do pagamento excede data máxima permitida (GPS)'                                                              .
    WHEN '057'. p_descricao =  'Pagamento duplicado (válido somente para o Canal Net Empresa)'                                                     .
    WHEN '058'. p_descricao =  'Para transação recusada com manutenção da sessão'                                                                  .
    WHEN '059'. p_descricao =  'Para transação recusada com encerramento da sessão'                                                                .
  ENDCASE.
ENDFORM.
