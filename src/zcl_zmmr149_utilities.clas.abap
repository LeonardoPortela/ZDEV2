*&-------------------------------------------------------------------------------------------------------*
*& Método         : GET_TEXTOS_FLXEXP_NF_SAIDA                                                           *
*& Chamado        : USER STORY 175240                                                                    *
*& Data           : 28/05/2025                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 28/05/2025|DEVK9A2K91 |NSEGATIN       | Compras de Insumos - Utilidades. Extração de dados. Dev. Inic.*
*--------------------------------------------------------------------------------------------------------*
class ZCL_ZMMR149_UTILITIES definition
  public
  final
  create public .

public section.

  methods SELECIONA_DADOS_EXTRACAO
    importing
      value(I_NRO_SOL_CP) type NUM10
    exporting
      !EE_ZMMT0035 type ZMMT0035
      !ET_ZMMT0036 type ZMMCT0036
      !ET_ZMMT0037 type ZMMCT0037
      !ET_ZMMT0038 type ZMMCT0038
      !EE_ZMMT0010 type ZMMT0010
      !ET_ZMMT0011 type ZMMCT0011
      !ET_ZMMT0012 type ZMMCT0012
      !E_MSG_ERRO type CHAR200 .
  methods VALIDA_DADOS_EXTRACAO
    importing
      !IE_ZMMT0035 type ZMMT0035
      !IT_ZMMT0036 type ZMMCT0036
      !IT_ZMMT0037 type ZMMCT0037
      !IT_ZMMT0038 type ZMMCT0038
      !IE_ZMMT0010 type ZMMT0010
      !IT_ZMMT0011 type ZMMCT0011
      !IT_ZMMT0012 type ZMMCT0012
    exporting
      !ET_MSG_VALIDA type BAPIRETTAB .
  methods VALIDA_DADOS_EXTRACAO_MSG
    importing
      value(I_MESSAGE) type STRING
      !IT_MSG_VALIDA type BAPIRETTAB
    exporting
      !ET_MSG_VALIDA type BAPIRETTAB .
  methods EXIBE_MSGS_VALIDA_DADOS_EXT
    importing
      !IT_MSG_VALIDA type BAPIRETTAB .
  methods PROCESSA_DADOS_EXTRACAO
    importing
      !IE_ZMMT0035 type ZMMT0035
      !IT_ZMMT0036 type ZMMCT0036
      !IT_ZMMT0037 type ZMMCT0037
      !IT_ZMMT0038 type ZMMCT0038
      !IE_ZMMT0010 type ZMMT0010
      !IT_ZMMT0011 type ZMMCT0011
      !IT_ZMMT0012 type ZMMCT0012
    exporting
      !E_MSG_RTN type CHAR200 .
  methods EXECUTA_EXTRACAO_DADOS
    importing
      value(I_NRO_SOL_CP) type NUM10
      !I_SHOW_MSG_VALIDA type CHAR_01 default SPACE
    exporting
      !E_MSG_RTN type CHAR200
      !ET_MSG_VALIDA type BAPIRETTAB .
  methods VALOR_ITEM
    importing
      value(IE_ZMMT0035) type ZMMT0035
      value(IE_ZMMT0037) type ZMMT0037
    returning
      value(R_VLR_ITEM) type NETWR .
  methods VALOR_TOTAL_ITENS
    importing
      !IE_ZMMT0035 type ZMMT0035
      !IT_ZMMT0037 type ZMMCT0037
    returning
      value(R_VLR_TOT_ITMS) type NETWR .
  methods CALCULO_IMPOSTO_ITEM
    importing
      !I_WERKS_0035 type EWERK
      !I_LIFNR type LIFNR
      value(I_WERKS) type EWERK
      !I_EBELP type EBELP
      !I_EBELN type EBELN
      !I_MATNR type MATNR
      !I_MENGE type BSTMG
      !I_NETPR type BPREI
      !I_MWSKZ type MWSKZ
      !I_PEINH type EPEIN
      !I_BPRME type BBPRM
      !I_NETPR_DESC type BPREI
      !I_NETPR_SUPL type BPREI
    exporting
      !E_VLR_ITEM type NETWR
      !E_VLR_IMP_IT type NETWR .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_ZMMR149_UTILITIES IMPLEMENTATION.


  METHOD calculo_imposto_item.

    TYPES: ty_konv TYPE TABLE OF komv.

    DATA: tl_konv TYPE TABLE OF konv.

    DATA: el_taxcom TYPE taxcom,
          el_ite    TYPE mepoitem.

    DATA: vl_lifnr_centro TYPE lifnr.

    FIELD-SYMBOLS: <wmwst> TYPE any,
                   <lfa1>  TYPE lfa1,
                   <ekpo>  TYPE ekpo,
                   <ekko>  TYPE ekko,
                   <vorga> TYPE any,
                   <konv>  TYPE ty_konv,
                   <cva>   TYPE any.

    CLEAR el_ite.

    CALL FUNCTION 'MEPO_DOC_ITEM_GET'
      EXPORTING
        im_ebelp = i_ebelp
      IMPORTING
        ex_item  = el_ite
      EXCEPTIONS
        failure  = 1
        OTHERS   = 2.

    ASSIGN ('(SAPLMEPO)ekpo') TO <ekpo>.
    ASSIGN ('(SAPLMEPO)ekko') TO <ekko>.
    ASSIGN ('(SAPLMEPO)lfa1') TO <lfa1>.

    CLEAR <ekpo>.
    SELECT SINGLE * FROM ekpo INTO <ekpo>
    WHERE ebeln EQ i_ebeln
      AND ebelp EQ i_ebelp.

    IF sy-subrc    IS NOT INITIAL OR
      <ekpo>-matnr NE i_matnr.
      IF i_werks IS INITIAL.
        i_werks = I_WERKS_0035.

      ENDIF.

      <ekpo>-werks = i_werks.

      SELECT SINGLE bukrs
        INTO <ekpo>-bukrs
        FROM j_1bbranch
      WHERE branch EQ <ekpo>-werks.

      SELECT SINGLE * FROM mara
        INTO @DATA(el_mara)
      WHERE  matnr EQ @i_matnr.

      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING el_mara TO <ekpo>.

      ENDIF.

      SELECT SINGLE * FROM mard
        INTO @DATA(el_mard)
      WHERE matnr EQ @i_matnr
        AND werks EQ @i_werks.

      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING el_mard TO <ekpo>.

      ENDIF.

      SELECT SINGLE * FROM mbew
        INTO @DATA(el_mbew)
      WHERE bwkey EQ @i_werks
        AND matnr EQ @i_matnr.

      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING el_mbew TO <ekpo>.

      ENDIF.

      SELECT SINGLE * FROM marc
        INTO @DATA(el_marc)
      WHERE werks EQ @i_werks
        AND matnr EQ @i_matnr.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING el_marc TO <ekpo>.

      ENDIF.

    ELSE.
      CLEAR <ekko>.
      SELECT SINGLE * FROM ekko INTO <ekko> WHERE ebeln EQ i_ebeln.

      SELECT SINGLE * FROM lfa1 INTO <lfa1> WHERE lifnr EQ <ekko>-lifnr.

    ENDIF.

    IF i_netpr GT 0 AND
       i_peinh GT 0.
      DATA(vl_fator) = 1000.

      IF i_bprme NE 'TO'.
        vl_fator = 1.

      ENDIF.

      IF <ekpo>-matnr NE i_matnr     AND
         i_matnr      IS NOT INITIAL.
        <ekpo>-matnr = i_matnr.

        SELECT SINGLE meins FROM mara
          INTO <ekpo>-meins
        WHERE matnr EQ i_matnr.

      ENDIF.

      IF i_werks IS INITIAL.
        <ekpo>-werks = i_werks_0035.

      ENDIF.

      vl_lifnr_centro = |{ <ekpo>-werks ALPHA = IN WIDTH = 10 }|.

      SELECT SINGLE * FROM lfa1 INTO @DATA(el_lifnr_cen) WHERE lifnr = @vl_lifnr_centro.

      IF i_lifnr IS NOT INITIAL.
        <ekko>-lifnr = i_lifnr.

      ENDIF.

      SELECT SINGLE * FROM lfa1 INTO <lfa1> WHERE lifnr = <ekko>-lifnr.

      <ekpo>-netpr = i_netpr.
      <ekpo>-menge = i_menge.
      <ekpo>-netwr = ( ( i_menge * i_netpr ) / i_peinh / vl_fator ) - i_netpr_desc + i_netpr_supl.
      <ekpo>-brtwr = <ekpo>-netwr.
      <ekpo>-effwr = <ekpo>-netwr.
      <ekpo>-bonba = <ekpo>-netwr.
      <ekpo>-mwskz = i_mwskz.
      <ekpo>-bprme = i_bprme.
      <ekpo>-txjcd = el_lifnr_cen-txjcd.
      <ekpo>-loekz = space.

    ENDIF.

    CLEAR <ekko>.
    SELECT SINGLE * FROM ekko INTO <ekko> WHERE ebeln EQ i_ebeln.

    IF <ekko>-bukrs IS INITIAL .
      SELECT SINGLE bukrs
        FROM j_1bbranch
        INTO  <ekko>-bukrs
      WHERE branch EQ <ekpo>-werks.

      <ekko>-bstyp = 'F'.
      <ekko>-lifnr = i_lifnr.
      <ekko>-waers = i_werks_0035.
      <ekko>-ekorg = 'OC01'.
      <ekko>-kalsm = 'RM0000'.
      <ekko>-aedat = sy-datum.
      <ekko>-bedat = sy-datum.

    ENDIF.

    IF <ekko>-knumv IS NOT INITIAL.
      SELECT * FROM konv INTO TABLE tl_konv WHERE knumv EQ <ekko>-knumv.

      ASSIGN ('(SAPLMEPO)tkomv[]') TO <konv>.
      <konv>[] = tl_konv[].
    ENDIF.


    IF <ekpo>-ebelp IS INITIAL     AND
       i_ebelp      IS NOT INITIAL.
      <ekpo>-ebelp = i_ebelp.

    ENDIF.

    ASSIGN ('(SAPLMEPO)fc_vorga') TO <vorga>.
    ASSIGN ('(SAPLMEPO)cva_en')   TO <cva>.

    <vorga> = <cva>.

    PERFORM kond_taxes IN PROGRAM saplmepo USING 'D' 'X'.

    CHECK <ekpo>-loekz EQ space.
    ASSIGN ('(SAPLMEPO)taxcom-WMWST') TO <wmwst>.

    DATA(lv_netwr) = <ekpo>-netwr.
    e_vlr_imp_it   = <wmwst>.
    e_vlr_item     = lv_netwr + <wmwst>.

  ENDMETHOD.


  METHOD executa_extracao_dados.
* Extrair dados para Contrato - Seleciona Dados
    seleciona_dados_extracao( EXPORTING i_nro_sol_cp = i_nro_sol_cp
                                IMPORTING ee_zmmt0035 = DATA(tl_zmmt0035)
                                          et_zmmt0036 = DATA(tl_zmmt0036)
                                          et_zmmt0037 = DATA(tl_zmmt0037)
                                          et_zmmt0038 = DATA(tl_zmmt0038)
                                          ee_zmmt0010 = DATA(tl_zmmt0010)
                                          et_zmmt0011 = DATA(tl_zmmt0011)
                                          et_zmmt0012 = DATA(tl_zmmt0012)
                                          e_msg_erro  = e_msg_rtn
                               ).

    IF NOT e_msg_rtn IS INITIAL.
      MESSAGE e_msg_rtn TYPE 'S' DISPLAY LIKE 'E'.

    ELSE.
* Extrair dados para Contrato - Valida Dados
      valida_dados_extracao( EXPORTING ie_zmmt0035   = tl_zmmt0035
                                       it_zmmt0036   = tl_zmmt0036
                                       it_zmmt0037   = tl_zmmt0037
                                       it_zmmt0038   = tl_zmmt0038
                                       ie_zmmt0010   = tl_zmmt0010
                                       it_zmmt0011   = tl_zmmt0011
                                       it_zmmt0012   = tl_zmmt0012
                             IMPORTING et_msg_valida = et_msg_valida
                            ).

      IF NOT et_msg_valida[]   IS INITIAL.
* Verifica se é para exibir a TI de mensagens da validação da extração.
        IF i_show_msg_valida IS INITIAL.
* Há criticas na validação dos dados de extração. Ver TI de mensagens.
          MESSAGE text-024 TYPE 'W'.

        ELSE.
* Exibe Mensagem da Validação de Dados para extração
          exibe_msgs_valida_dados_ext( EXPORTING it_msg_valida = et_msg_valida ).

        ENDIF.


      ELSE.
* Extrair dados para Contrato - Processa Dados
        processa_dados_extracao( EXPORTING ie_zmmt0035 = tl_zmmt0035
                                           it_zmmt0036 = tl_zmmt0036
                                           it_zmmt0037 = tl_zmmt0037
                                           it_zmmt0038 = tl_zmmt0038
                                           ie_zmmt0010 = tl_zmmt0010
                                           it_zmmt0011 = tl_zmmt0011
                                           it_zmmt0012 = tl_zmmt0012
                                 IMPORTING e_msg_rtn   = e_msg_rtn
                                ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD exibe_msgs_valida_dados_ext.
* Verifica se há mensagens a serem exibidas.
    IF NOT it_msg_valida[] IS INITIAL.
* Exibe mensagens de processamentos.
      CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
        EXPORTING
          it_message = it_msg_valida[].

    ENDIF.

  ENDMETHOD.


  METHOD processa_dados_extracao.

    DATA: tl_zsdt0311 TYPE TABLE OF zsdt0311,
          tl_zsdt0312 TYPE TABLE OF zsdt0312,
          tl_zsdt0013 TYPE TABLE OF zsdt0013,
          tl_zsdt0014 TYPE TABLE OF zsdt0014,
          tl_zsdt0015 TYPE TABLE OF zsdt0015,
          tl_line     TYPE          tline_t.

    DATA: el_zsdt0310 TYPE zsdt0310,
          el_zsdt0311 TYPE zsdt0311,
          el_zsdt0312 TYPE zsdt0312,
          el_zsdt0013 TYPE zsdt0013,
          el_zsdt0014 TYPE zsdt0014,
          el_zsdt0015 TYPE zsdt0015.

    DATA: vl_name TYPE tdobname.
* Marca para Cancelamento se o status for Gerar Documento para gerar um novo.
    UPDATE zsdt0310
       SET status     = '10' "Cancelado
           usnam_canc = sy-uname
           data_canc  = sy-datlo
           hora_canc  = sy-timlo
    WHERE nr_venda EQ ie_zmmt0035-nro_sol_cp
      AND status   EQ '00'. "Gerar Documento
*** CABEÇALHO DO DOCUMENTO
    el_zsdt0310-mandt                  = sy-mandt.
* Gera numeração sequêncial do documento.
    PERFORM f_gera_seq IN PROGRAM saplzgfsd001 USING    'ZSD_ID_DOC'
                                               CHANGING el_zsdt0310-id_documento .
    el_zsdt0310-id_doc_agrupador       = '0'.
    el_zsdt0310-area                   = 'IN'.
    el_zsdt0310-tipo_doc               = 'CTC'.
    el_zsdt0310-nr_venda               = ie_zmmt0035-nro_sol_cp.
    el_zsdt0310-vbeln                  = space.
    el_zsdt0310-tpsim                  = space.
    SELECT SINGLE vkorg FROM t001w INTO el_zsdt0310-vkorg WHERE werks EQ ie_zmmt0035-werks.
    IF sy-subrc IS INITIAL.
      IF     el_zsdt0310-vkorg+1(1) EQ '0'.
        el_zsdt0310-vkbur = |{ el_zsdt0310-vkorg+2(2) }01|.

      ELSEIF el_zsdt0310-vkorg+1(1) NE '0'.
        el_zsdt0310-vkbur = |F{ el_zsdt0310-vkorg+1(1) }01|.

      ENDIF.

    ENDIF.

    el_zsdt0310-dt_vencimento          = ie_zmmt0010-data_final_pagamento.
    el_zsdt0310-fazenda                = ie_zmmt0035-ped_forn.
    el_zsdt0310-area_ha                = space.
    el_zsdt0310-waerk                  = ie_zmmt0035-waers.
    el_zsdt0310-kursf                  = ie_zmmt0035-wkurs.
    el_zsdt0310-tpcult                 = ie_zmmt0035-cultura.
    DATA(vl_len) = strlen( ie_zmmt0035-safra ) - 4.
    el_zsdt0310-safra                  = ie_zmmt0035-safra+vl_len(4).
    el_zsdt0310-area_penhor            = space.
    DATA(vl_vlrtot)                    = valor_total_itens( EXPORTING ie_zmmt0035 = ie_zmmt0035
                                                                      it_zmmt0037 = it_zmmt0037 ).
    el_zsdt0310-vlrtot                 = COND #( WHEN ie_zmmt0035-waers = 'BRL' THEN vl_vlrtot
                                                                                ELSE vl_vlrtot * ie_zmmt0035-wkurs ).
    el_zsdt0310-vlrtot_usd             = COND #( WHEN ie_zmmt0035-waers = 'BRL' THEN 0
                                                                                ELSE vl_vlrtot ).
    el_zsdt0310-juros_ano              = space.
    el_zsdt0310-pag_prorrogado         = space.
    el_zsdt0310-tipo_doc_digital       = abap_off.
    DATA(vl_lifnr) = |{ ie_zmmt0035-lifnr ALPHA = IN WIDTH = 10 }|.
    SELECT SINGLE bankl, bankn FROM but0bk INTO ( @DATA(vl_bankl), @el_zsdt0310-conta ) WHERE partner EQ @vl_lifnr
                                                                                          AND bkvid   EQ @ie_zmmt0035-bvtyp.
    el_zsdt0310-banco                  = vl_bankl(3).
    el_zsdt0310-agencia                = vl_bankl+5.
    el_zsdt0310-status                 = '00'.
    el_zsdt0310-usname                 = sy-uname.
    el_zsdt0310-data                   = sy-datum.
    el_zsdt0310-hora                   = sy-uzeit.
    el_zsdt0310-id_assinatura          = space.
    el_zsdt0310-nr_doc_gerado          = space.
    el_zsdt0310-assinatura_sequencial  = space.
    el_zsdt0310-proibir_rejeicao       = space.
    el_zsdt0310-observacao             = space.
    el_zsdt0310-canal_distribuicao     = space.
    el_zsdt0310-imp_valor_unit         = space.
    el_zsdt0310-tp_simulador           = COND #( WHEN ie_zmmt0035-zterm = 'ZPAR' THEN 'PARCELADA'
                                                 WHEN ie_zmmt0035-zterm = 'Z002' THEN '72 HORAS'
                                                 WHEN ie_zmmt0035-zterm = 'Z007' THEN 'SEMANAL'
                                                 WHEN ie_zmmt0035-zterm = 'Z015' THEN 'QUINZEAL'
                                                 WHEN ie_zmmt0035-zterm = 'Z030' THEN 'MENSAL'
                                                                                 ELSE space ).
    el_zsdt0310-forma_pagamento        = ie_zmmt0035-forma_pagamento .
    el_zsdt0310-local_entrega          = ie_zmmt0035-local_entrega.
    el_zsdt0310-multa_compradora       = ie_zmmt0010-multa_compradora.
    el_zsdt0310-multa_vendedora        = ie_zmmt0010-multa_vendedora.
    el_zsdt0310-royalties              = ie_zmmt0010-royalties.
    el_zsdt0310-alocacao_royalties     = ie_zmmt0010-alocacao_royalties.
    el_zsdt0310-dias_prazo             = ie_zmmt0010-dias_prazo_reclamacao.
    el_zsdt0310-data_doc_gerado        = space.
    el_zsdt0310-hora_doc_gerado        = space.
    el_zsdt0310-usnam_canc             = space.
    el_zsdt0310-data_canc              = space.
    el_zsdt0310-hora_canc              = space.

*** GERADOR DE DOCUMENTOS - PARTICIPANTES
    DATA(vl_seq) = 1.
* Emitente
    el_zsdt0311-mandt        = sy-mandt.
    el_zsdt0311-id_documento = el_zsdt0310-id_documento.
    el_zsdt0311-seq_kunnr    = vl_seq.
    el_zsdt0311-kunnr        = |{ ie_zmmt0035-lifnr ALPHA = IN WIDTH = 10 }|.
    el_zsdt0311-tp_emitente  = 'EMITENTE'.
    APPEND el_zsdt0311 TO tl_zsdt0311.
    CLEAR el_zsdt0311.

    ADD 1 TO vl_seq.
* Fiador
    el_zsdt0311-mandt        = sy-mandt.
    el_zsdt0311-id_documento = el_zsdt0310-id_documento.
    el_zsdt0311-seq_kunnr    = vl_seq.
    el_zsdt0311-kunnr        = ie_zmmt0010-cod_fiador.
    el_zsdt0311-tp_emitente  = 'FIADOR'.
    APPEND el_zsdt0311 TO tl_zsdt0311.
    CLEAR: el_zsdt0311, vl_seq.

    LOOP AT it_zmmt0037 INTO DATA(el_zmmt0037) WHERE nro_sol_cp EQ ie_zmmt0035-nro_sol_cp.
      ADD 1 TO vl_seq.
*** GERADOR DE DOCUMENTOS - ITENS
      el_zsdt0312-mandt             = sy-mandt.
      el_zsdt0312-id_documento      = el_zsdt0310-id_documento.
      el_zsdt0312-seq_item          = vl_seq.
      el_zsdt0312-vbeln             = space.
      el_zsdt0312-posnr             = vl_seq.

      SELECT SINGLE a~matkl, b~spart, c~maktx
        FROM mara AS a
         INNER JOIN zmmt0200 AS b
          ON a~matkl EQ b~matkl
         INNER JOIN makt AS c
          ON a~matnr EQ c~matnr
        INTO @DATA(e_spart)
      WHERE a~matnr EQ @el_zmmt0037-matnr.

      el_zsdt0312-tp_insumo         = COND #( WHEN e_spart-spart = '02' THEN 'FET'
                                              WHEN e_spart-spart = '03' THEN 'DEF'
                                              WHEN e_spart-spart = '04' THEN 'SEM' ).
      el_zsdt0312-inco1             = ie_zmmt0035-inco1.
      el_zsdt0312-matnr             = el_zmmt0037-matnr.

      IF NOT el_zmmt0037-inscricao_campo IS INITIAL.
        SELECT SINGLE nome FROM zsdt0199 INTO @DATA(vl_nome) WHERE id_cat_sementes EQ @el_zmmt0037-id_cat_sementes.
        IF sy-subrc IS INITIAL.
          el_zsdt0312-desc_matnr = |{ e_spart-maktx }- CLASSE: { vl_nome }|.

        ENDIF.

      ELSE.
        el_zsdt0312-desc_matnr = e_spart-maktx.

      ENDIF.

      el_zsdt0312-werks             = COND #( WHEN el_zmmt0037-werks IS INITIAL THEN ie_zmmt0035-werks ELSE el_zmmt0037-werks ).

      IF ie_zmmt0035-local_entrega IS INITIAL.
        IF NOT el_zmmt0037-rota_pc IS INITIAL.
          SELECT SINGLE * FROM zsdt0132 INTO @DATA(el_ZSDT0132) WHERE nr_rot EQ @el_zmmt0037-rota_pc.
          IF sy-subrc IS INITIAL.
            el_zsdt0312-endereco_entrega = |{ el_zsdt0132-endereco }, NO MUNICIPIO DE: { el_zsdt0132-city1 }, ESTADO DE: { el_zsdt0132-uf }|.

          ENDIF.

        ENDIF.

      ELSE.
        DATA(vl_local_entrega) = |{ ie_zmmt0035-local_entrega ALPHA = IN WIDTH = 10 }|.
        SELECT SINGLE * FROM lfa1 INTO @DATA(el_lfa1) WHERE lifnr EQ @vl_local_entrega.
        IF sy-subrc IS INITIAL.
          el_zsdt0312-endereco_entrega = |{ el_lfa1-stras } - BAIRRO: { el_lfa1-ort02 } NO MUNICIPIO DE: { el_lfa1-ort01 } ESTADO DE: { el_lfa1-regio }|.

        ENDIF.

      ENDIF.

      el_zsdt0312-zmeng             = el_zmmt0037-menge.

      vl_name = el_zmmt0037-matnr.
* Busca o texto da Unidade de Medida para ver se está cadastrado.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id              = 'GRUN'
          language        = sy-langu
          name            = vl_name
          object          = 'MATERIAL'
        TABLES
          lines           = tl_line
        EXCEPTIONS
          object          = 1
          id              = 2
          language        = 3
          name            = 4
          not_found       = 5
          reference_check = 6.

      TRY.
          el_zsdt0312-descricao_um = tl_line[ 1 ]-tdline.

        CATCH cx_sy_itab_line_not_found INTO DATA(l_error).
          CLEAR el_zsdt0312-descricao_um.

      ENDTRY.

      el_zsdt0312-vlr_unit          = el_zmmt0037-brtwr.
      el_zsdt0312-vlr_total         = valor_item( EXPORTING ie_zmmt0035 = ie_zmmt0035
                                                            ie_zmmt0037 = el_zmmt0037 ).
      IF line_exists( it_zmmt0038[ nro_sol_cp = el_zmmt0037-nro_sol_cp ebelp = el_zmmt0037-ebelp ] ).
        DATA(le_zmmt0038) = it_zmmt0038[ nro_sol_cp = el_zmmt0037-nro_sol_cp ebelp = el_zmmt0037-ebelp ].

      ELSE.
        CLEAR le_zmmt0038.

      ENDIF.

      el_zsdt0312-dt_entrega        = le_zmmt0038-data_progr_de.
      el_zsdt0312-dt_entrega_ate    = le_zmmt0038-data_progr.
      el_zsdt0312-safra_aplicacao   = space.
      el_zsdt0312-valor_frete       = space.
      el_zsdt0312-cultura_aplicacao = space.
      el_zsdt0312-tp_ov             = space.

      APPEND el_zsdt0312 TO tl_zsdt0312.

    ENDLOOP.

*** GERADOR DE DOCUMENTOS - PROGRAMAÇÃO DE PAGAMENTOS
    LOOP AT it_zmmt0036 INTO DATA(el_zmmt0036) WHERE nro_sol_cp EQ ie_zmmt0035-nro_sol_cp.
      el_zsdt0013-mandt           = sy-mandt.
      el_zsdt0013-id_documento    = el_zsdt0310-id_documento.
      el_zsdt0013-dt_vcto         = el_zmmt0036-dt_vcto.
      el_zsdt0013-modo_pagamento  = el_zmmt0036-modo_pagamento.
      el_zsdt0013-zterm           = el_zmmt0036-zterm.
      el_zsdt0013-percentual      = el_zmmt0036-percentual.
      el_zsdt0013-valor           = COND #( WHEN ie_zmmt0035-waers = 'BRL' THEN el_zmmt0036-valor
                                                                           ELSE el_zmmt0036-valor * ie_zmmt0035-wkurs ).
      el_zsdt0013-valor_usd       = COND #( WHEN ie_zmmt0035-waers = 'BRL' THEN 0
                                                                           ELSE el_zmmt0036-valor ).

      APPEND el_zsdt0013 TO tl_zsdt0013.

    ENDLOOP.

*** GERADOR DE DOCUMENTOS - DADOS DE ROYALTIES
    LOOP AT it_zmmt0011 INTO DATA(el_zmmt0011) WHERE nro_sol_cp EQ ie_zmmt0035-nro_sol_cp
                                                 AND id_seq     EQ ie_zmmt0010-id_seq.
      el_zsdt0014-mandt               = sy-mandt.
      el_zsdt0014-id_documento        = el_zsdt0310-id_documento.
      el_zsdt0014-cod_obtentora       = el_zmmt0011-cod_obtentora.
      el_zmmt0011-partner = |{ el_zmmt0011-partner ALPHA = IN WIDTH = 10 }|.
      SELECT SINGLE bankl, bankn FROM but0bk INTO ( @vl_bankl, @el_zsdt0014-conta ) WHERE partner EQ @el_zmmt0011-partner
                                                                                      AND bkvid   EQ @el_zmmt0011-bkvid.
      el_zsdt0014-banco               = vl_bankl(3).
      el_zsdt0014-agencia             = vl_bankl+5.
      el_zsdt0014-valor_royalties     = COND #( WHEN ie_zmmt0035-waers = 'BRL' THEN el_zmmt0011-valor_royalties
                                                                               ELSE el_zmmt0011-valor_royalties * ie_zmmt0035-wkurs ).
      el_zsdt0014-valor_royalties_usd = COND #( WHEN ie_zmmt0035-waers = 'BRL' THEN 0
                                                                               ELSE el_zsdt0014-valor_royalties ).
      el_zsdt0014-mes                 = el_zmmt0011-mes.
      el_zsdt0014-ano                 = el_zmmt0011-ano.

      APPEND el_zsdt0014 TO tl_zsdt0014.

    ENDLOOP.
*** GERADOR DE DOCUMENTOS - TESTEMUNHAS
    LOOP AT it_zmmt0012 INTO DATA(el_ZMMT0012) WHERE nro_sol_cp EQ ie_zmmt0035-nro_sol_cp
                                                 AND id_seq     EQ ie_zmmt0010-id_seq.
      el_zsdt0015-mandt        = sy-mandt.
      el_zsdt0015-id_documento = el_zsdt0310-id_documento.
      el_zsdt0015-cpf          = el_zmmt0012-cpf.
      el_zsdt0015-nome         = el_zmmt0012-nome.
      el_zsdt0015-email        = el_zmmt0012-email.

      APPEND el_zsdt0015 TO tl_zsdt0015.

    ENDLOOP.
*** GRAVA DADOS NA TABELA.
    MODIFY zsdt0310 FROM el_zsdt0310.
    MODIFY zsdt0311 FROM TABLE tl_zsdt0311.
    MODIFY zsdt0312 FROM TABLE tl_zsdt0312.
    MODIFY zsdt0013 FROM TABLE tl_zsdt0013.
    MODIFY zsdt0014 FROM TABLE tl_zsdt0014.
    MODIFY zsdt0015 FROM TABLE tl_zsdt0015.
    COMMIT WORK.
    e_msg_rtn = 'Extração de dados para contrado finalizada. Verificar!'.

  ENDMETHOD.


  METHOD seleciona_dados_extracao.

    SELECT SINGLE * FROM zmmt0035 INTO ee_zmmt0035 WHERE nro_sol_cp EQ i_nro_sol_cp.

    IF sy-subrc IS INITIAL.
      IF ee_zmmt0035-loekz IS NOT INITIAL.
        e_msg_erro = 'Nº de Solicitação foi eliminada!'.

      ELSE.
        SELECT * FROM zmmt0036 INTO TABLE et_zmmt0036  WHERE nro_sol_cp EQ i_nro_sol_cp.
        SELECT * FROM zmmt0037 INTO TABLE et_zmmt0037  WHERE nro_sol_cp EQ i_nro_sol_cp.
        SELECT * FROM zmmt0038 INTO TABLE et_zmmt0038  WHERE nro_sol_cp EQ i_nro_sol_cp.
        SELECT SINGLE * FROM zmmt0010 INTO ee_zmmt0010 WHERE nro_sol_cp EQ i_nro_sol_cp
                                                         AND cancel     EQ space.
        SELECT * FROM zmmt0011 INTO TABLE et_zmmt0011  WHERE nro_sol_cp EQ i_nro_sol_cp
                                                         AND id_seq     EQ ee_zmmt0010-id_seq
                                                         AND cancel     EQ space.
        SELECT * FROM zmmt0012 INTO TABLE et_zmmt0012  WHERE nro_sol_cp EQ i_nro_sol_cp
                                                         AND id_seq     EQ ee_zmmt0010-id_seq
                                                         AND cancel     EQ space.

      ENDIF.

    ELSE.
      e_msg_erro = 'Nº de Solicitação não encontrada!'.

    ENDIF.

  ENDMETHOD.


  METHOD valida_dados_extracao.

    DATA: lt_line   TYPE tline_t,
          tl_values TYPE TABLE OF rgsb4.

    DATA: vl_name TYPE tdobname.

* Busca os dados do Set de Validação do Código da Moeda.
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr      = 'ZMMT0149_CODIGO_MOEDA'
        class      = '0000'
        table      = 'ZMMT0035'
        fieldname  = 'WAERS'
      TABLES
        set_values = tl_values.

    IF tl_values[] IS INITIAL.
* Não há código de moeda cadastrado. Cadastrar! (Transação GS02 - SET ZMMT0149_CODIGO_MOEDA)
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-026 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).
* A Extração de dados para contrato não será gerada
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-023 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).
      RETURN.

    ELSE.
      IF NOT line_exists( tl_values[ from = ie_zmmt0035-waers ] ).
* Código de Moeda & não permitido
        DATA(vl_message) = CONV string( TEXT-027 ).
        REPLACE '&' IN vl_message WITH ie_zmmt0035-waers.
        valida_dados_extracao_msg( EXPORTING i_message     = CONV #( vl_message )
                                             it_msg_valida = et_msg_valida
                                   IMPORTING et_msg_valida = et_msg_valida
                                  ).
* A Extração de dados para contrato não será gerada
        valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-023 )
                                             it_msg_valida = et_msg_valida
                                   IMPORTING et_msg_valida = et_msg_valida
                                  ).
        RETURN.

      ENDIF.

    ENDIF.
* Valida se Documento de Pedido do Contrato já existe.
    SELECT SINGLE * FROM zsdt0310 INTO @DATA(el_t0310) WHERE nr_venda   EQ @ie_zmmt0035-nro_sol_cp
                                                         AND status     NE '10'
                                                         AND usnam_canc EQ @space.

    IF sy-subrc        IS INITIAL AND
       el_t0310-status NE '00'.
      SELECT SINGLE ddtext FROM dd07t INTO @DATA(vl_ddtext) WHERE domname    EQ 'ZSTATUS_DOC'
                                                              AND ddlanguage EQ 'P'
                                                              AND domvalue_l EQ @el_t0310-status.

      IF sy-subrc IS INITIAL.
* Já existe extração realizada e esta se encontra em Status &. Verificar pela Transação ZSDT0203.
        vl_message = CONV string( TEXT-025 ).
        REPLACE '&' IN vl_message WITH vl_ddtext.
        valida_dados_extracao_msg( EXPORTING i_message     = CONV #( vl_message )
                                             it_msg_valida = et_msg_valida
                                   IMPORTING et_msg_valida = et_msg_valida
                                  ).
* A Extração de dados para contrato não será gerada
        valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-023 )
                                             it_msg_valida = et_msg_valida
                                   IMPORTING et_msg_valida = et_msg_valida
                                  ).
        RETURN.

      ENDIF.

    ENDIF.

    IF ie_zmmt0035-werks IS INITIAL.
* A filial não foi informada e devido a isso não é possível Determinar a empresa para emissão do Contrato
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-001 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0035-lifnr IS INITIAL.
* O Fornecedor não foi informado
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-002 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0035-cultura IS INITIAL.
* A Cultura não foi informada
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-003 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0035-ped_forn IS INITIAL.
* O Nº do pedido do Fornecedor não foi informado
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-004 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0035-bvtyp IS INITIAL.
* Os dados bancários do Vendedor não foi informado
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-005 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0035-forma_pagamento EQ sy-abcde+5(1). "F - Fracionado
      IF NOT line_exists( it_zmmt0036[ nro_sol_cp = ie_zmmt0035-nro_sol_cp ] ).
* Não foram localizados os lançamentos das Parcelas do pagamento Fracionado
        valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-006 )
                                             it_msg_valida = et_msg_valida
                                   IMPORTING et_msg_valida = et_msg_valida
                                  ).

      ENDIF.

    ENDIF.

    IF ie_zmmt0035-inco1         NE 'FOB' AND
       ie_zmmt0035-local_entrega IS INITIAL.
* O Frete é & , logo o Local de Entregar é Obrigatório
      vl_message = CONV string( TEXT-007 ).
      REPLACE '&' IN vl_message WITH ie_zmmt0035-inco1.
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( vl_message )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0035-inco1 EQ 'FOB'.
      LOOP AT it_zmmt0037 INTO DATA(el_zmmt0037) WHERE rota_pc IS INITIAL.
* O Frete é FOB, logo o Roteiro do Ponto de Coleta para o Item & é Obrigatório
        vl_message = CONV string( TEXT-008 ).
        REPLACE '&' IN vl_message WITH el_zmmt0037-ebeln.
        valida_dados_extracao_msg( EXPORTING i_message     = CONV #( vl_message )
                                             it_msg_valida = et_msg_valida
                                   IMPORTING et_msg_valida = et_msg_valida
                                  ).

      ENDLOOP.

      LOOP AT it_zmmt0037 INTO el_zmmt0037 WHERE rota_pc IS NOT INITIAL.
        DATA(vl_nr_rot) = |{ el_zmmt0037-rota_pc ALPHA = IN WIDTH = 10 }|.
        SELECT SINGLE city1, uf, endereco
          FROM zsdt0132
          INTO @DATA(el_rota_pc)
        WHERE nr_rot EQ @vl_nr_rot
          AND status EQ @sy-abcde(1). "A - Ativo

        IF   sy-subrc IS INITIAL            AND
           ( el_rota_pc-city1    IS INITIAL OR
             el_rota_pc-uf       IS INITIAL OR
             el_rota_pc-endereco IS INITIAL ).
* Cadastros do roteiro imcompleto. Atualizar na transação ZSDT0113.
          valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-028 )
                                               it_msg_valida = et_msg_valida
                                     IMPORTING et_msg_valida = et_msg_valida
                                    ).

        ENDIF.

      ENDLOOP.

    ENDIF.

    IF ie_zmmt0035-zterm IS INITIAL.
* Não foi informada a condição de Pagamento!
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-009 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF NOT line_exists( it_zmmt0038[ nro_sol_cp = ie_zmmt0035-nro_sol_cp ] ).
* Não foram localizados os lançamentos da Programação de Entrega
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-010 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF NOT line_exists( it_zmmt0037[ nro_sol_cp = ie_zmmt0035-nro_sol_cp ] ).
* Não foi localizado nenhum item na Solicitação do Pedido de Compra
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-011 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).
* A Extração de dados para contrato não será gerada
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-023 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).
      RETURN.

    ENDIF.

    LOOP AT it_zmmt0037 INTO el_zmmt0037.
      IF el_zmmt0037-brtwr IS INITIAL.
* Não foram informado Preço Bruto do item &
        vl_message = CONV string( TEXT-012 ).
        REPLACE '&' IN vl_message WITH el_zmmt0037-ebelp.
        valida_dados_extracao_msg( EXPORTING i_message     = CONV #( vl_message )
                                             it_msg_valida = et_msg_valida
                                   IMPORTING et_msg_valida = et_msg_valida
                                  ).

      ENDIF.

      IF el_zmmt0037-netpr IS INITIAL.
* Não foi calculado o Valor Final do item &
        vl_message = CONV string( TEXT-013 ).
        REPLACE '&' IN vl_message WITH el_zmmt0037-ebeln.
        valida_dados_extracao_msg( EXPORTING i_message     = CONV #( vl_message )
                                             it_msg_valida = et_msg_valida
                                   IMPORTING et_msg_valida = et_msg_valida
                                  ).

      ENDIF.

      vl_name = el_zmmt0037-matnr.
* Busca o texto da Unidade de Medida para ver se está cadastrado.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id              = 'GRUN'
          language        = sy-langu
          name            = vl_name
          object          = 'MATERIAL'
        TABLES
          lines           = lt_line
        EXCEPTIONS
          object          = 1
          id              = 2
          language        = 3
          name            = 4
          not_found       = 5
          reference_check = 6.

      TRY.
          vl_message = lt_line[ 1 ]-tdline.

        CATCH cx_sy_itab_line_not_found INTO DATA(l_error).
          CLEAR vl_message.

      ENDTRY.

      IF vl_message IS INITIAL.
* Não foi localizado no Cadastro do Material &1 do item &2 a informação da Unidade de Medida utilizada em Documentos
        vl_message = CONV string( TEXT-014 ).
        DATA(vl_matnr) = | { el_zmmt0037-matnr ALPHA = OUT } |.
        DATA(vl_ebelp) = | { el_zmmt0037-ebelp ALPHA = OUT } |.
        CONDENSE vl_matnr NO-GAPS.
        CONDENSE vl_ebelp NO-GAPS.
        DATA(vl_len) = strlen( vl_matnr ).
        REPLACE '&1' IN vl_message WITH vl_matnr(vl_len).
        vl_len = strlen( vl_ebelp ).
        REPLACE '&2' IN vl_message WITH vl_ebelp(vl_len).
        valida_dados_extracao_msg( EXPORTING i_message     = CONV #( vl_message )
                                             it_msg_valida = et_msg_valida
                                   IMPORTING et_msg_valida = et_msg_valida
                                  ).

      ENDIF.

    ENDLOOP.

    IF ie_zmmt0010 IS INITIAL.
* Não foram localizados informações da Aba "Dados para Contrato"
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-015 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).
* A Extração de dados para contrato não será gerada
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-023 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).
      RETURN.

    ENDIF.

    IF ie_zmmt0010-data_final_pagamento IS INITIAL       AND
       ie_zmmt0035-forma_pagamento      NE sy-abcde+15(1).   "P - Pós-Embarque
* Não foi informada a data Final de Pagamento
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-016 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0010-dias_prazo_reclamacao IS INITIAL.
* Não foi informado os dias para o prazo de reclamação da qualidade do produto
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-017 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).
* A Extração de dados para contrato não será gerada
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-023 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).
      RETURN.

    ENDIF.

    IF ie_zmmt0010-cod_fiador      IS INITIAL       AND
       ie_zmmt0035-forma_pagamento NE sy-abcde+15(1).   "P - Pós-Embarque
* Não foi informado o Fiador
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-018 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0010-multa_compradora IS INITIAL.
* Não foi informado % da Multa para a parte Compradora
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-019 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0010-multa_vendedora IS INITIAL.
* Não foi informado % da Multa para a parte Compradora
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-020 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF ie_zmmt0010-royalties          EQ abap_on                                                        AND
       ie_zmmt0010-alocacao_royalties EQ sy-abcde+2(1)                                                  AND  "C - Compradora
       NOT line_exists( it_zmmt0011[ nro_sol_cp = ie_zmmt0035-nro_sol_cp id_seq = ie_zmmt0010-id_seq ] ).
* A alocação dos Royalties está para a Compradora, mas os dados para pagamentos não foram localizados
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-021 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF NOT line_exists( it_zmmt0012[ nro_sol_cp = ie_zmmt0035-nro_sol_cp id_seq = ie_zmmt0010-id_seq ] ).
* Não foram localizados dados de Testemunha do Vendedor
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-022 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

    IF NOT et_msg_valida[] IS INITIAL.
* A Extração de dados para contrato não será gerada
      valida_dados_extracao_msg( EXPORTING i_message     = CONV #( TEXT-023 )
                                           it_msg_valida = et_msg_valida
                                 IMPORTING et_msg_valida = et_msg_valida
                                ).

    ENDIF.

  ENDMETHOD.


  METHOD valida_dados_extracao_msg.

    DATA: tl_message TYPE sdokhttptext.

    et_msg_valida = it_msg_valida.

    CALL FUNCTION 'SPLIT_MESSAGE'
      EXPORTING
        max_length  = 50
        message     = i_message
      TABLES
        message_tab = tl_message.

    DATA(lv_row) = lines( et_msg_valida ) + 1.

    APPEND INITIAL LINE TO et_msg_valida ASSIGNING FIELD-SYMBOL(<fs_msg>).
    <fs_msg>-type   = sy-abcde+4(1). "E
    <fs_msg>-id     = '00'.
    <fs_msg>-number = '398'.
    <fs_msg>-row    = lv_row.

    IF line_exists( tl_message[ 1 ]  ).
      <fs_msg>-message_v1  = tl_message[ 1 ]-line.

    ENDIF.

    IF line_exists( tl_message[ 2 ] ).
      <fs_msg>-message_v2  = tl_message[ 2 ]-line.

    ENDIF.

    IF line_exists( tl_message[ 3 ] ).
      <fs_msg>-message_v3  = tl_message[ 3 ]-line.

    ENDIF.

    IF line_exists( tl_message[ 4 ] ).
      <fs_msg>-message_v4  = tl_message[ 4 ]-line.

    ENDIF.

  ENDMETHOD.


  METHOD valor_item.

    DATA(vl_ebeln) = ie_zmmt0035-ebeln.

    IF ie_zmmt0037-ebeln IS NOT INITIAL.
      vl_ebeln = ie_zmmt0037-ebeln.

    ENDIF.

    SELECT SINGLE *
      FROM ekpo
      INTO @DATA(el_ekpo)
      WHERE ebeln = @vl_ebeln
      AND   ebelp = @ie_zmmt0037-ebelp.

    IF ie_zmmt0037-peinh GT 0.
      DATA(vl_fator) = 1000.

      IF ie_zmmt0037-bprme NE 'TO'.
        vl_fator = 1.

      ENDIF.

      IF el_ekpo-loekz NE 'L'.
        r_vlr_item = ( ie_zmmt0037-menge * ( ( ie_zmmt0037-netpr ) / ie_zmmt0037-peinh ) ) / vl_fator.

      ENDIF.

    ELSE.
      r_vlr_item = 0.

    ENDIF.

    SELECT SINGLE * FROM ekko INTO @DATA(el_ekko) WHERE ebeln EQ @vl_ebeln.

    IF sy-subrc IS INITIAL.
      calculo_imposto_item( EXPORTING i_werks_0035 = ie_zmmt0035-werks
                                      i_lifnr      = el_ekko-lifnr
                                      i_werks      = el_ekpo-werks
                                      i_ebelp      = el_ekpo-ebelp
                                      i_ebeln      = el_ekpo-ebeln
                                      i_matnr      = el_ekpo-matnr
                                      i_menge      = el_ekpo-menge
                                      i_netpr      = 0
                                      i_mwskz      = el_ekpo-mwskz
                                      i_peinh      = el_ekpo-peinh
                                      i_bprme      = el_ekpo-bprme
                                      i_netpr_desc = 0
                                      i_netpr_supl = 0
                            IMPORTING e_vlr_item   = r_vlr_item
                          ).

      IF r_vlr_item        IS INITIAL AND
         ie_zmmt0037-peinh GT 0.
        r_vlr_item = ( ie_zmmt0037-menge * ( ( ie_zmmt0037-netpr ) / ie_zmmt0037-peinh ) ) / vl_fator.

      ENDIF.

    ELSE.
      calculo_imposto_item( EXPORTING i_werks_0035 = ie_zmmt0035-werks
                                      i_lifnr      = el_ekko-lifnr
                                      i_werks      = el_ekpo-werks
                                      i_ebelp      = ie_zmmt0037-ebelp
                                      i_ebeln      = vl_ebeln
                                      i_matnr      = ie_zmmt0037-matnr
                                      i_menge      = ie_zmmt0037-menge
                                      i_netpr      = CONV #( ie_zmmt0037-netpr )
                                      i_mwskz      = ie_zmmt0037-mwskz
                                      i_peinh      = ie_zmmt0037-peinh
                                      i_bprme      = ie_zmmt0037-bprme
                                      i_netpr_desc = ie_zmmt0037-netpr_desc
                                      i_netpr_supl = ie_zmmt0037-netpr_supl
                            IMPORTING e_vlr_item   = r_vlr_item
                          ).

    ENDIF.

  ENDMETHOD.


  METHOD valor_total_itens.

    LOOP AT it_zmmt0037 INTO DATA(el_zmmt0037) WHERE nro_sol_cp EQ ie_zmmt0035-nro_sol_cp.
      DATA(vl_vlr_item) = valor_item( EXPORTING ie_zmmt0035 = ie_zmmt0035
                                                ie_zmmt0037 = el_zmmt0037 ).

      r_vlr_tot_itms +=  vl_vlr_item.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
