************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 14.05.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Geração de Arquivo de Tributos                      *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 14.05.2009    Desenvolvedor ABAP   Criação              DEVK905828   *
************************************************************************

REPORT  zimp08.

TABLES: zimp_tipos_impos.
*-----------------------------------------------------------------------
* Tipos
*-----------------------------------------------------------------------
TYPES:

  BEGIN OF ty_tp_arrec_1_e_2,
    ident_tributo               TYPE c,
    nome_cliente(40)            TYPE c,
    end_cliente(40)             TYPE c,
    cep(8)                      TYPE c,
    uf_cliente(2)               TYPE c,
    cidade_cliente(20)          TYPE c,
    bairro_cliente(20)          TYPE c,
    tipo_inscricao              TYPE c,
    num_inscricao(15)           TYPE c,
    tel_cliente_pagador(20)     TYPE c,
    data_pagamento_tributo(8)   TYPE n,
    autoriza_pagamento          TYPE c,
    valor_principal(15)         TYPE n,
    valor_juros_encargos(15)    TYPE n,
    valor_multa(15)             TYPE n,
    reserva_1(15)               TYPE c,
    data_venc_tributo(8)        TYPE n,
    codigo_receita(4)           TYPE n,
    periodo_apuracao(8)         TYPE n,
    percentual(4)               TYPE n,
    referencia(17)              TYPE c,
    receita_bruta_acumulada(15) TYPE n,
    uso_empresa(80)             TYPE c,
    reserva(49)                 TYPE c,
    agencia(4)                  TYPE n,
    conta(7)                    TYPE n,
    mestre(8)                   TYPE n,
    reserva_2(23)               TYPE c,
* Início Alteração Ricardo Furst 07.07.2009
*    cr_lf(2)                    TYPE c,
* Fim Alteração Ricardo Furst 07.07.2009
  END OF ty_tp_arrec_1_e_2,

  BEGIN OF ty_tp_arrec_3,
    ident_tributo               TYPE c,
    nome_cliente(40)            TYPE c,
    end_cliente(40)             TYPE c,
    cep(8)                      TYPE c,
    uf_cliente(2)               TYPE c,
    cidade_cliente(20)          TYPE c,
    bairro_cliente(20)          TYPE c,
    tipo_inscricao              TYPE c,
    num_inscricao(15)           TYPE c,
    tel_cliente_pagador(20)     TYPE c,
    data_pagamento_tributo(8)   TYPE n,
    autoriza_pagamento          TYPE c,
    valor_inss(15)              TYPE n,
    valor_monet_juros_multa(15) TYPE n,
    valor_outras_entidades(15)  TYPE n,
    valor_total(15)             TYPE n,
    codigo_receita(4)           TYPE n,
    tipo_identificador(2)       TYPE c,
    identificador(14)           TYPE c,
    ano_competencia(4)          TYPE c,
    mes_competencia(2)          TYPE c,
    uso_empresa(80)             TYPE c,
    nome_recolhedor(40)         TYPE c,
    reserva_1(39)               TYPE c,
    agencia(4)                  TYPE n,
    conta(7)                    TYPE n,
    mestre(8)                   TYPE n,
    reserva_2(23)               TYPE c,
* Início Alteração Ricardo Furst 07.07.2009
*    cr_lf(2)                    TYPE c,
* Fim Alteração Ricardo Furst 07.07.2009
  END OF ty_tp_arrec_3,

  BEGIN OF ty_tp_arrec_4_e_5,
    ident_tributo               TYPE c,
    nome_cliente(40)            TYPE c,
    end_cliente(40)             TYPE c,
    cep(8)                      TYPE c,
    uf_cliente(2)               TYPE c,
    cidade_cliente(20)          TYPE c,
    bairro_cliente(20)          TYPE c,
    tipo_inscricao              TYPE c,
    num_inscricao(15)           TYPE c,
    tel_cliente_pagador(20)     TYPE c,
    data_pagamento_tributo(8)   TYPE n,
    autoriza_pagamento          TYPE c,
    valor_principal(15)         TYPE n,
    valor_juros_encargos(15)    TYPE n,
    valor_multa(15)             TYPE n,
    valor_acrescimo_fin(15)     TYPE n,
    valor_honorarios_adv(15)    TYPE n,
    reserva_1(17)               TYPE c,
    data_venc_tributo(8)        TYPE n,
    codigo_receita(4)           TYPE n,
    grupo_receita               TYPE c,
    mes(2)                      TYPE n,
    ano(4)                      TYPE n,
    inscricao_estadual(12)      TYPE c,
    inscricao_divida_ativa(13)  TYPE c,
    numero_parcelamento(13)     TYPE n,
    reserva_2(2)                TYPE c,
    uso_empresa(40)             TYPE c,
    observacoes(40)             TYPE c,
    cnae(7)                     TYPE c,
    placa(7)                    TYPE c,
    agencia(4)                  TYPE n,
    conta(7)                    TYPE n,
    mestre(8)                   TYPE n,
    reserva(23)                 TYPE c,
* Início Alteração Ricardo Furst 07.07.2009
*    cr_lf(2)                    TYPE c,
* Fim Alteração Ricardo Furst 07.07.2009
  END OF ty_tp_arrec_4_e_5,

  BEGIN OF ty_tp_arrec_6,
    ident_tributo               TYPE c,
    cod_barras(48)              TYPE n,
    data_pagamento_tributo(8)   TYPE n,
    data_vencimento(8)          TYPE n,
    reserva(298)                TYPE c,
    agencia(4)                  TYPE n,
    conta(7)                    TYPE n,
    mestre(8)                   TYPE n,
    autoriza_pagamento(1)       TYPE c,
    uso_empresa(80)             TYPE c,
* Início Alteração Ricardo Furst 07.07.2009
*    cr_lf(2)                    TYPE c,
* Fim Alteração Ricardo Furst 07.07.2009
  END OF ty_tp_arrec_6,

  BEGIN OF ty_tp_arrec_7,
    ident_tributo               TYPE c,
    cod_barras(48)              TYPE n,
    data_pagamento_tributo(8)   TYPE n,
    data_vencimento(8)          TYPE n,
    valor_multa(15)             TYPE n,
    valor_juros(15)             TYPE n,
    reserva_1(45)               TYPE n,
    identificador(20)           TYPE c,
    reserva_2(203)              TYPE c,
    agencia(4)                  TYPE n,
    conta(7)                    TYPE n,
    mestre(8)                   TYPE n,
    autoriza_pagamento(1)       TYPE c,
    uso_empresa(80)             TYPE c,
* Início Alteração Ricardo Furst 07.07.2009
*    cr_lf(2)                    TYPE c,
* Fim Alteração Ricardo Furst 07.07.2009
  END OF ty_tp_arrec_7,

  BEGIN OF ty_tp_arrec_8_9_10_11,
    ident_tributo               TYPE c,
*---> 30/05/2023 - Migração S4 - JS
    "cod_barras(48)              TYPE n,
    "data_pagamento_tributo(8)   TYPE n,
    cod_barras(48)              TYPE c,
    data_pagamento_tributo      TYPE sy-datum,
*<--- 30/05/2023 - Migração S4 - JS
    reserva_1(15)               TYPE c,
    autoriza_pagamento(1)       TYPE c,
    reserva_2(291)              TYPE c,
    agencia(4)                  TYPE n,
    conta(7)                    TYPE n,
    mestre(8)                   TYPE n,
    uso_empresa(80)             TYPE c,
* Início Alteração Ricardo Furst 07.07.2009
*    cr_lf(2)                    TYPE c,
* Fim Alteração Ricardo Furst 07.07.2009
  END OF ty_tp_arrec_8_9_10_11 ,

  BEGIN OF ty_tp_arrec_12,
    ident_tributo               TYPE c,
    reserva_1(146)              TYPE c,
    data_pagamento_tributo(8)   TYPE n,
    autoriza_pagamento(1)       TYPE c,
    valor_principal(15)         TYPE n,
    valor_juros(15)             TYPE n,
    valor_multa(15)             TYPE n,
    valor_at_monet(15)          TYPE n,
    cod_barras(48)              TYPE n,
    data_vencimento(8)          TYPE n,
    reserva_2(63)               TYPE c,
    agencia(4)                  TYPE n,
    conta(7)                    TYPE n,
    mestre(8)                   TYPE n,
    uso_empresa(40)             TYPE c,
    reserva_3(69)               TYPE c,
* Início Alteração Ricardo Furst 07.07.2009
*    cr_lf(2)                    TYPE c,
* Fim Alteração Ricardo Furst 07.07.2009
  END OF ty_tp_arrec_12,

  BEGIN OF ty_tp_arrec_13,
    ident_tributo               TYPE c,
    nome_cliente(40)            TYPE c,
    end_cliente(40)             TYPE c,
    cep(8)                      TYPE c,
    uf_cliente(2)               TYPE c,
    cidade_cliente(20)          TYPE c,
    tipo_inscricao              TYPE c,
    num_inscricao(15)           TYPE c,
    tel_cliente_pagador(20)     TYPE c,
    data_pagamento_tributo(8)   TYPE n,
    autoriza_pagamento          TYPE c,
    valor_principal(15)         TYPE n,
    valor_juros(15)             TYPE n,
    valor_multa(15)             TYPE n,
    valor_at_monet(15)          TYPE n,
    cod_barras(48)              TYPE n,
    data_vencimento_tributo(8)  TYPE n,
    codigo_receita(6)           TYPE n,
    uf_favorecida(2)            TYPE c,
    num_doc_origem(13)          TYPE c,
    reserva_1(1)                TYPE c,
    referencia(1)               TYPE c,
    mes(2)                      TYPE n,
    ano(4)                      TYPE c,
    reserva_2                   TYPE c,
    inscricao_estadual(12)      TYPE c,
    convenio(40)                TYPE c,
    uso_empresa(40)             TYPE c,
    produto(2)                  TYPE n,
    reserva_3(21)               TYPE c,
    codigo_sub_receita(2)       TYPE c,
    reserva_4(44)               TYPE c,
* Início Alteração Ricardo Furst 07.07.2009
*    cr_lf(2)                    TYPE c,
* Fim Alteração Ricardo Furst 07.07.2009
  END OF ty_tp_arrec_13,

* Início Alteração Ricardo Furst 07.07.2009
*  ty_arquivo(465)               TYPE c.
  ty_arquivo(463)               TYPE c.
* Fim Alteração Ricardo Furst 07.07.2009
*-----------------------------------------------------------------------
* Estruturas
*-----------------------------------------------------------------------
DATA:

  w_arquivo                     TYPE ty_arquivo,
  p_unix_old                    TYPE char1,
  w_tp_arrec_1_e_2              TYPE ty_tp_arrec_1_e_2,
  w_tp_arrec_3                  TYPE ty_tp_arrec_3,
  w_tp_arrec_4_e_5              TYPE ty_tp_arrec_4_e_5,
  w_tp_arrec_6                  TYPE ty_tp_arrec_6,
  w_tp_arrec_7                  TYPE ty_tp_arrec_7,
  w_tp_arrec_8_9_10_11          TYPE ty_tp_arrec_8_9_10_11,
  w_tp_arrec_12                 TYPE ty_tp_arrec_12,
  w_tp_arrec_13                 TYPE ty_tp_arrec_13,

  w_adrc                        TYPE adrc,
* Início Alteração Ricardo Furst 07.07.2009
  w_t012k                       TYPE t012k.
* Fim Alteração Ricardo Furst 07.07.2009

*-----------------------------------------------------------------------
* Tabelas internas
*-----------------------------------------------------------------------
DATA:

  t_arquivo                       TYPE TABLE OF ty_arquivo,
  t_arquivo1                      TYPE TABLE OF ty_arquivo,
  t_arquivo2                      TYPE TABLE OF ty_arquivo,
  t_arquivo3                      TYPE TABLE OF ty_arquivo,
  t_arquivo4                      TYPE TABLE OF ty_arquivo,
  t_arquivo5                      TYPE TABLE OF ty_arquivo,
  t_arquivo6                      TYPE TABLE OF ty_arquivo,
  t_arquivo7                      TYPE TABLE OF ty_arquivo,
  t_arquivo8                      TYPE TABLE OF ty_arquivo,
  t_arquivo9                      TYPE TABLE OF ty_arquivo,
  t_arquivo10                     TYPE TABLE OF ty_arquivo,
  t_arquivo11                     TYPE TABLE OF ty_arquivo,
  t_arquivo12                     TYPE TABLE OF ty_arquivo,
  t_arquivo13                     TYPE TABLE OF ty_arquivo,

  t_zimp_cabecalho              TYPE TABLE OF zimp_cabecalho,
  t_zimp_detalhe                TYPE SORTED TABLE OF zimp_detalhe
                                  WITH NON-UNIQUE KEY nro_doc_tr gjahr.

*-----------------------------------------------------------------------
* Símbolos de campo
*-----------------------------------------------------------------------
FIELD-SYMBOLS:

  <f_zimp_cabecalho>            TYPE zimp_cabecalho,
  <f_zimp_detalhe>              TYPE zimp_detalhe.

*-----------------------------------------------------------------------
* Variáveis
*-----------------------------------------------------------------------
DATA:

  v_butxt                       TYPE t001-butxt,

  v_regs_proc                   TYPE i,
  v_arqs_proc                   TYPE i.

* Início Alteração Ricardo Furst 07.07.2009
TYPES: BEGIN OF ty_log,
        bldat     TYPE bkpf-bldat,
        blart     TYPE bkpf-blart,
        bukrs     TYPE ekko-bukrs,
        budat     TYPE bkpf-budat,
        waers     TYPE ekko-waers,
        konto     TYPE rf05a-konto,
        hbkid     TYPE t012k-hbkid,
        gsber     TYPE ekpo-werks,
        wrbtr(16) TYPE c,
        sel01     TYPE rf05a-sel01,
        agkon     TYPE rf05a-agkon,
        tipo(1)   TYPE c,
        msg(100)  TYPE c,
  END OF ty_log.

DATA: BEGIN OF t_batch OCCURS 0,
        bldat     TYPE bkpf-bldat,
        blart     TYPE bkpf-blart,
        bukrs     TYPE ekko-bukrs,
        budat     TYPE bkpf-budat,
        waers     TYPE ekko-waers,
        konto     TYPE rf05a-konto,
        hbkid     TYPE t012k-hbkid,
        gsber     TYPE ekpo-werks,
        wrbtr(16) TYPE c,
        sel01     TYPE rf05a-sel01,
        agkon     TYPE rf05a-agkon,
        data(10)      TYPE c,
  belnr TYPE bkpf-belnr,
  gjahr TYPE bkpf-gjahr,
      END OF t_batch.

DATA: ti_log TYPE STANDARD TABLE OF ty_log,
      ti_bdc TYPE STANDARD TABLE OF bdcdata,
      ti_msg TYPE STANDARD TABLE OF bdcmsgcoll.

DATA: wa_log LIKE LINE OF ti_log,
      wa_bdc LIKE LINE OF ti_bdc,
      wa_msg LIKE LINE OF ti_msg.

DATA: v_msg           LIKE t100-text,
* Inclusão - Inicio - KZORZAN - ROLLOUT - 30.07.2009 ******************
      v_budat         LIKE bkpf-budat,
* Inclusão - Final - KZORZAN - ROLLOUT - 30.07.2009 *******************
      v_msgv1         LIKE balm-msgv1,
      v_msgv2         LIKE balm-msgv2,
      v_msgv3         LIKE balm-msgv3,
      v_msgv4         LIKE balm-msgv4,
      v_mode          TYPE c VALUE 'N',
      v_tit           TYPE zimp_detalhe-vlr_principal,
      v_tot_tit       TYPE zimp_detalhe-vlr_principal,
      lv_nome_arquivo TYPE string.
* Fim Alteração Ricardo Furst 07.07.2009

*-----------------------------------------------------------------------
* Parâmetros de seleção
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-t01.

PARAMETERS:
  p_bncemp                      TYPE t012-hbkid DEFAULT 'BBD'.

SELECT-OPTIONS:
  s_nr_doc                      FOR <f_zimp_detalhe>-nro_doc_tr
                                , "NO-EXTENSION," NO INTERVALS,
  s_lifnr                       FOR <f_zimp_detalhe>-lifnr.

PARAMETERS:
  p_emp                         TYPE b120-bukrs
                                OBLIGATORY.

SELECT-OPTIONS:
  s_dtlanc                      FOR <f_zimp_cabecalho>-budat,

  s_dtvenc                      FOR <f_zimp_cabecalho>-zfbdt OBLIGATORY,
  s_tparrc                      FOR zimp_tipos_impos-tp_arrec
                                NO INTERVALS OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-t02.
PARAMETERS:  p_pc   TYPE char1 RADIOBUTTON GROUP rgr1 DEFAULT 'X' USER-COMMAND usr,
             p_unix TYPE char1 RADIOBUTTON GROUP rgr1.

PARAMETERS: p_path(250) DEFAULT 'C:\' LOWER CASE.
SELECTION-SCREEN END OF BLOCK b02.

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

**
AT SELECTION-SCREEN OUTPUT.
  IF p_unix_old <> p_unix.
    p_unix_old = p_unix.
    IF p_pc = 'X'.
      p_path = 'C:\'.
    ELSE.
      p_path = '/sap/usr/'.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  IF p_pc = 'X'.
    CALL FUNCTION 'WS_FILENAME_GET'
      EXPORTING
        def_filename     = ' '
        def_path         = 'C:\'
        mask             = '*.TXT'
        mode             = 'S'
        title            = 'Busca de Arquivo'
      IMPORTING
        filename         = p_path
      EXCEPTIONS
        inv_winsys       = 1
        no_batch         = 2
        selection_cancel = 3
        selection_error  = 4
        OTHERS           = 5.
  ELSE.
    DATA: wl_path TYPE dxlpath.
    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
    EXPORTING
      i_location_flag = ' '
*      i_server = lv_servername
      i_path = '//'
      filemask = '*.*'
      fileoperation = 'R'
    IMPORTING
*     O_LOCATION_FLAG =
*     O_SERVER =
      o_path = wl_path
*     ABEND_FLAG =
    EXCEPTIONS
      rfc_error = 1
      OTHERS = 2.
    MOVE wl_path TO p_path.

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tparrc-low.
  PERFORM match_code_tparrc.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM zf_carrega_tabelas.

  PERFORM zf_monta_arquivo.

  PERFORM zf_informacao.

  PERFORM zf_batch_input.

*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_TABELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_carrega_tabelas .

  SELECT *
    INTO TABLE t_zimp_cabecalho
    FROM zimp_cabecalho
    WHERE bukrs       = p_emp    AND
          nro_doc_tr IN s_nr_doc AND
          budat      IN s_dtlanc AND
          zfbdt      IN s_dtvenc AND
          lifnr      IN s_lifnr  AND
          tp_arrec   IN s_tparrc AND
          belnr      <> space    AND
          arq_gerado  = space.

  IF sy-subrc IS INITIAL.
    SELECT *
      INTO TABLE t_zimp_detalhe
      FROM zimp_detalhe
      FOR ALL ENTRIES IN t_zimp_cabecalho
      WHERE bukrs      = p_emp AND
            nro_doc_tr = t_zimp_cabecalho-nro_doc_tr AND
            aprovador  = 'X'.
  ELSE.
*    MESSAGE i208(00) WITH 'Não existe dados para esta seleção'.
*    STOP.
  ENDIF.

ENDFORM.                    " ZF_CARREGA_TABELAS
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_monta_arquivo .

* Início Alteração Ricardo Furst 07.07.2009
  CLEAR: v_tot_tit.
* Fim Alteração Ricardo Furst 07.07.2009

  LOOP AT t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.

    REFRESH t_arquivo.

    LOOP AT t_zimp_detalhe ASSIGNING <f_zimp_detalhe>
      WHERE nro_doc_tr = <f_zimp_cabecalho>-nro_doc_tr AND
            gjahr      = <f_zimp_cabecalho>-gjahr.

      CASE <f_zimp_cabecalho>-tp_arrec.

        WHEN '01' OR '02'.
          PERFORM zf_monta_tp_01_02.
          IF <f_zimp_cabecalho>-tp_arrec = '01'.
            APPEND w_arquivo TO t_arquivo1.
          ELSE.
            APPEND w_arquivo TO t_arquivo2.
          ENDIF.

        WHEN '03'.
          PERFORM zf_monta_tp_03.
          APPEND w_arquivo TO t_arquivo3.

        WHEN '04' OR '05'.
          PERFORM zf_monta_tp_04_05.
          IF <f_zimp_cabecalho>-tp_arrec = '04'.
            APPEND w_arquivo TO t_arquivo4.
          ELSE.
            APPEND w_arquivo TO t_arquivo5.
          ENDIF.

        WHEN '06'.
          PERFORM zf_monta_tp_06.
          APPEND w_arquivo TO t_arquivo6.

        WHEN '07'.
          PERFORM zf_monta_tp_07.
          APPEND w_arquivo TO t_arquivo7.

        WHEN '08' OR '09' OR '10' OR '11'.
          PERFORM zf_monta_tp_8_9_10_11.
          CASE <f_zimp_cabecalho>-tp_arrec.
            WHEN '08'.
              APPEND w_arquivo TO t_arquivo8.
            WHEN '09'.
              APPEND w_arquivo TO t_arquivo9.
            WHEN '10'.
              APPEND w_arquivo TO t_arquivo10.
            WHEN '11'.
              APPEND w_arquivo TO t_arquivo11.
          ENDCASE.

        WHEN '12'.
          PERFORM zf_monta_tp_12.
          APPEND w_arquivo TO t_arquivo12.

        WHEN '13'.
          PERFORM zf_monta_tp_13.
          APPEND w_arquivo TO t_arquivo13.

      ENDCASE.

* Início Alteração Ricardo Furst 07.07.2009
      v_tit = ( <f_zimp_detalhe>-vlr_principal  +
                <f_zimp_detalhe>-vlr_juros      +
                <f_zimp_detalhe>-vlr_multa      +
                <f_zimp_detalhe>-vlr_outras_ent +
                <f_zimp_detalhe>-tse            +
                <f_zimp_detalhe>-vlr_atual_mone ).

      v_tot_tit = ( v_tot_tit                       +
                    <f_zimp_detalhe>-vlr_principal  +
                    <f_zimp_detalhe>-vlr_juros      +
                    <f_zimp_detalhe>-vlr_multa      +
                    <f_zimp_detalhe>-vlr_outras_ent +
                    <f_zimp_detalhe>-tse            +
                    <f_zimp_detalhe>-vlr_atual_mone ).
* Fim Alteração Ricardo Furst 07.07.2009

* Inclusão - Inicio - KZORZAN - ROLLOUT - 30.07.2009 ******************
      CLEAR v_budat.
      WRITE <f_zimp_cabecalho>-zfbdt TO t_batch-data.
* Inclusão - Inicio - KZORZAN - ROLLOUT - 30.07.2009 ******************

      APPEND w_arquivo TO t_arquivo.

      v_regs_proc = v_regs_proc + 1.

      PERFORM f_tbatch.

    ENDLOOP.

  ENDLOOP.

  IF '08' IN s_tparrc.
    PERFORM zf_monta_tp_8.
  ENDIF.


  PERFORM zf_grava_arquivo.

ENDFORM.                    " ZF_MONTA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_01_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_monta_tp_01_02 .

  CLEAR w_tp_arrec_1_e_2.

  w_tp_arrec_1_e_2-ident_tributo = <f_zimp_cabecalho>-tp_arrec+1.

  PERFORM zf_nome_cliente USING  w_tp_arrec_1_e_2-num_inscricao.

  w_tp_arrec_1_e_2-nome_cliente   = v_butxt.
  w_tp_arrec_1_e_2-end_cliente    = w_adrc-street.
  w_tp_arrec_1_e_2-cep            = w_adrc-post_code1.
  w_tp_arrec_1_e_2-uf_cliente     = w_adrc-region.
  w_tp_arrec_1_e_2-cidade_cliente = w_adrc-city1.
  w_tp_arrec_1_e_2-bairro_cliente = w_adrc-city2.
  w_tp_arrec_1_e_2-tipo_inscricao = '2'.
  w_tp_arrec_1_e_2-tel_cliente_pagador = w_adrc-tel_number.
  w_tp_arrec_1_e_2-data_pagamento_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_1_e_2-autoriza_pagamento = 'S'.
  w_tp_arrec_1_e_2-valor_principal = <f_zimp_detalhe>-vlr_principal * 100.
  w_tp_arrec_1_e_2-valor_juros_encargos = <f_zimp_detalhe>-vlr_juros * 100.
  w_tp_arrec_1_e_2-valor_multa = <f_zimp_detalhe>-vlr_multa * 100.
  w_tp_arrec_1_e_2-data_venc_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_1_e_2-codigo_receita = <f_zimp_cabecalho>-cod_pgto.
  w_tp_arrec_1_e_2-periodo_apuracao = <f_zimp_cabecalho>-dt_per_apur.
  w_tp_arrec_1_e_2-referencia = <f_zimp_detalhe>-cod_ident.

  PERFORM zf_banco_agencia USING w_tp_arrec_1_e_2-agencia
                                 w_tp_arrec_1_e_2-conta.

*  w_tp_arrec_1_e_2-cr_lf = cl_abap_char_utilities=>cr_lf.
*Início Alteração Ricardo Furst 07.06.2009
*  w_tp_arrec_1_e_2-cr_lf = '..'.
  w_tp_arrec_1_e_2-reserva_2 = '                      .'.
*Fim Alteração Ricardo Furst 07.06.2009

  w_arquivo = w_tp_arrec_1_e_2.

ENDFORM.                    " ZF_MONTA_TP_01_02
*&---------------------------------------------------------------------*
*&      Form  ZF_NOME_CLIENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_nome_cliente USING p_cgc TYPE char15.

  DATA:
    lv_adrnr TYPE t001-adrnr,
    lv_cgc_number TYPE j_1bwfield-cgc_number.


  SELECT SINGLE adrnr butxt
    INTO (lv_adrnr, v_butxt)
    FROM t001
    WHERE bukrs = p_emp.

  SELECT SINGLE *
    INTO w_adrc
    FROM adrc
    WHERE addrnumber = lv_adrnr   AND
          date_from  = '00010101' AND
          nation     = space.

* Início Alteração Ricardo Furst 14.07.2009
  IF sy-subrc = 0.

    REPLACE '-' WITH space INTO w_adrc-post_code1.
    CONDENSE w_adrc-post_code1 NO-GAPS.
    CONCATENATE '0'
                w_adrc-post_code1
           INTO w_adrc-post_code1.

  ENDIF.
* Fim Alteração Ricardo Furst 14.07.2009

  CALL FUNCTION 'J_1BREAD_CGC_COMPANY'
    EXPORTING
      bukrs      = p_emp
    IMPORTING
      cgc_number = lv_cgc_number.

  p_cgc = lv_cgc_number.

ENDFORM.                    " ZF_NOME_CLIENTE
*&---------------------------------------------------------------------*
*&      Form  ZF_BANCO_AGENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_banco_agencia  USING    p_agencia TYPE any
                                p_conta   TYPE any.

  DATA: wl_agencia TYPE bankk,
        wl_conta   TYPE bankn.
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

  MOVE: wl_conta TO p_conta,
        wl_agencia TO p_agencia.

ENDFORM.                    " ZF_BANCO_AGENCIA
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_monta_tp_03 .

  CLEAR w_tp_arrec_3.

*  w_tp_arrec_3-ident_tributo = <f_zimp_cabecalho>-tp_arrec+1.
  w_tp_arrec_3-ident_tributo = '7'.

  PERFORM zf_nome_cliente USING  w_tp_arrec_3-num_inscricao.

  w_tp_arrec_3-nome_cliente   = v_butxt.
  w_tp_arrec_3-end_cliente    = w_adrc-street.
  w_tp_arrec_3-cep            = w_adrc-post_code1.
  w_tp_arrec_3-uf_cliente     = w_adrc-region.
  w_tp_arrec_3-cidade_cliente = w_adrc-city1.
  w_tp_arrec_3-bairro_cliente = w_adrc-city2.
  w_tp_arrec_3-tipo_inscricao = '2'.
  w_tp_arrec_3-tel_cliente_pagador = w_adrc-tel_number.
  w_tp_arrec_3-data_pagamento_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_3-autoriza_pagamento = 'S'.
  w_tp_arrec_3-valor_inss = <f_zimp_detalhe>-vlr_principal * 100.
  w_tp_arrec_3-valor_monet_juros_multa = ( <f_zimp_detalhe>-vlr_juros +
                                           <f_zimp_detalhe>-vlr_multa ) * 100.
  w_tp_arrec_3-valor_outras_entidades = <f_zimp_detalhe>-vlr_outras_ent * 100.
  w_tp_arrec_3-valor_total = ( <f_zimp_detalhe>-vlr_principal +
                               <f_zimp_detalhe>-vlr_juros     +
                               <f_zimp_detalhe>-vlr_multa     +
                               <f_zimp_detalhe>-vlr_outras_ent ) * 100.
  w_tp_arrec_3-codigo_receita = <f_zimp_cabecalho>-cod_pgto.
  w_tp_arrec_3-tipo_identificador = '01'.
  w_tp_arrec_3-identificador = <f_zimp_detalhe>-cod_ident.

  w_tp_arrec_3-ano_competencia = <f_zimp_cabecalho>-mes_ano_comp(4).
  w_tp_arrec_3-mes_competencia = <f_zimp_cabecalho>-mes_ano_comp+4.

  IF <f_zimp_cabecalho>-cod_pgto = '2631' OR
     <f_zimp_cabecalho>-cod_pgto = '2658'.

    w_tp_arrec_3-nome_recolhedor = v_butxt.

  ENDIF.

  PERFORM zf_banco_agencia USING w_tp_arrec_3-agencia
                                 w_tp_arrec_3-conta.

*  w_tp_arrec_3-cr_lf = cl_abap_char_utilities=>cr_lf.
*Início Alteração Ricardo Furst 07.06.2009
*  w_tp_arrec_3-cr_lf = '..'.
  w_tp_arrec_3-reserva_2 = '                      .'.
*Fim Alteração Ricardo Furst 07.06.2009

  w_arquivo = w_tp_arrec_3.

ENDFORM.                    " ZF_MONTA_TP_03
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_04_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_monta_tp_04_05 .

  CLEAR w_tp_arrec_4_e_5.

  CASE <f_zimp_cabecalho>-tp_arrec.

    WHEN '04'.
      w_tp_arrec_4_e_5-ident_tributo = '5'.

    WHEN '05'.
      w_tp_arrec_4_e_5-ident_tributo = '6'.

  ENDCASE.

  PERFORM zf_nome_cliente USING  w_tp_arrec_4_e_5-num_inscricao.

  w_tp_arrec_4_e_5-nome_cliente   = v_butxt.
  w_tp_arrec_4_e_5-end_cliente    = w_adrc-street.
  w_tp_arrec_4_e_5-cep            = w_adrc-post_code1.
  w_tp_arrec_4_e_5-uf_cliente     = w_adrc-region.
  w_tp_arrec_4_e_5-cidade_cliente = w_adrc-city1.
  w_tp_arrec_4_e_5-bairro_cliente = w_adrc-city2.
  w_tp_arrec_4_e_5-tipo_inscricao = '2'.
  w_tp_arrec_4_e_5-tel_cliente_pagador = w_adrc-tel_number.
  w_tp_arrec_4_e_5-data_pagamento_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_4_e_5-autoriza_pagamento = 'S'.
  w_tp_arrec_4_e_5-valor_principal = <f_zimp_detalhe>-vlr_principal * 100.
  w_tp_arrec_4_e_5-valor_juros_encargos = <f_zimp_detalhe>-vlr_juros * 100.
  w_tp_arrec_4_e_5-valor_multa = <f_zimp_detalhe>-vlr_multa * 100.
  w_tp_arrec_4_e_5-data_venc_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_4_e_5-codigo_receita = <f_zimp_cabecalho>-cod_pgto.
  w_tp_arrec_4_e_5-ano = <f_zimp_cabecalho>-mes_ano_comp(4).
  w_tp_arrec_4_e_5-mes = <f_zimp_cabecalho>-mes_ano_comp+4.

  PERFORM zf_banco_agencia USING w_tp_arrec_4_e_5-agencia
                                 w_tp_arrec_4_e_5-conta.

*  w_tp_arrec_4_e_5-cr_lf = cl_abap_char_utilities=>cr_lf.
*Início Alteração Ricardo Furst 07.06.2009
*  w_tp_arrec_4_e_5-cr_lf = '..'.
  w_tp_arrec_4_e_5-reserva_2 = '                      .'.
*Fim Alteração Ricardo Furst 07.06.2009
  w_arquivo = w_tp_arrec_4_e_5.

ENDFORM.                    " ZF_MONTA_TP_04_05
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_monta_tp_06 .

  CLEAR w_tp_arrec_6.

  w_tp_arrec_6-ident_tributo = 'R'.

  w_tp_arrec_6-cod_barras = <f_zimp_detalhe>-cod_barras.
  w_tp_arrec_6-data_pagamento_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_6-data_vencimento = <f_zimp_cabecalho>-zfbdt.


  PERFORM zf_banco_agencia USING w_tp_arrec_6-agencia
                                 w_tp_arrec_6-conta.

  w_tp_arrec_6-autoriza_pagamento = 'S'.

*  w_tp_arrec_6-cr_lf = cl_abap_char_utilities=>cr_lf.
*Início Alteração Ricardo Furst 07.06.2009
*  w_tp_arrec_6-cr_lf = '..'.
  w_tp_arrec_6-uso_empresa = '                                                                               .'.
*Fim Alteração Ricardo Furst 07.06.2009

  w_arquivo = w_tp_arrec_6.

ENDFORM.                    " ZF_MONTA_TP_06
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_07
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_monta_tp_07 .

  CLEAR w_tp_arrec_7.

  w_tp_arrec_7-ident_tributo = 'F'.

  w_tp_arrec_7-cod_barras = <f_zimp_detalhe>-cod_barras.
  w_tp_arrec_7-data_pagamento_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_7-data_vencimento = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_7-valor_multa = <f_zimp_detalhe>-vlr_multa.
  w_tp_arrec_7-valor_juros = <f_zimp_detalhe>-vlr_juros.
  w_tp_arrec_7-identificador = <f_zimp_detalhe>-cod_ident.

  PERFORM zf_banco_agencia USING w_tp_arrec_7-agencia
                                 w_tp_arrec_7-conta.

  w_tp_arrec_7-autoriza_pagamento = 'S'.

*  w_tp_arrec_7-cr_lf = cl_abap_char_utilities=>cr_lf.

*Início Alteração Ricardo Furst 07.06.2009
*  w_tp_arrec_7-cr_lf = '..'.
  w_tp_arrec_7-uso_empresa = '                                                                               .'.
*Fim Alteração Ricardo Furst 07.06.2009

  w_arquivo = w_tp_arrec_7.

ENDFORM.                    " ZF_MONTA_TP_07
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_8_9_10_11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_monta_tp_8_9_10_11 .

  CLEAR w_tp_arrec_8_9_10_11.

  DATA: w_valor(15) TYPE n.

  CASE <f_zimp_cabecalho>-tp_arrec.

    WHEN '08'.
      w_tp_arrec_8_9_10_11-ident_tributo = 'B'.

    WHEN '09'.
      w_tp_arrec_8_9_10_11-ident_tributo = 'D'.
      w_valor = <f_zimp_detalhe>-vlr_principal * 100.
      w_tp_arrec_8_9_10_11-reserva_1 = w_valor.
    WHEN '10'.
      w_tp_arrec_8_9_10_11-ident_tributo = 'I'.
    WHEN '11'.
      w_tp_arrec_8_9_10_11-ident_tributo = 'O'.

  ENDCASE.


  IF <f_zimp_cabecalho>-tp_arrec EQ '08'.

    SELECT SINGLE cod_barras zfbdt FROM zimp_contas_cons
      INTO (w_tp_arrec_8_9_10_11-cod_barras, w_tp_arrec_8_9_10_11-data_pagamento_tributo)
      WHERE bukrs EQ <f_zimp_cabecalho>-bukrs  AND
            belnr EQ <f_zimp_cabecalho>-belnr.

*    w_tp_arrec_8_9_10_11-cod_barras = <f_zimp_detalhe>-cod_barras.
*    w_tp_arrec_8_9_10_11-data_pagamento_tributo = <f_zimp_cabecalho>-zfbdt.
  ELSE.
    w_tp_arrec_8_9_10_11-cod_barras = <f_zimp_detalhe>-cod_barras.
    w_tp_arrec_8_9_10_11-data_pagamento_tributo = <f_zimp_cabecalho>-zfbdt.
    w_tp_arrec_8_9_10_11-autoriza_pagamento = 'S'.
  ENDIF.




  PERFORM zf_banco_agencia USING w_tp_arrec_8_9_10_11-agencia
                                 w_tp_arrec_8_9_10_11-conta.

*  w_tp_arrec_8_9_10_11-cr_lf = cl_abap_char_utilities=>cr_lf.
*Início Alteração Ricardo Furst 07.06.2009
*  w_tp_arrec_8_9_10_11-cr_lf = '.'.
  w_tp_arrec_8_9_10_11-uso_empresa = '                                                                               .'.
*Fim Alteração Ricardo Furst 07.06.2009

  w_arquivo = w_tp_arrec_8_9_10_11.

ENDFORM.                    " ZF_MONTA_TP_8_9_10_11
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_12
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_monta_tp_12 .

  CLEAR w_tp_arrec_12.

  w_tp_arrec_12-ident_tributo = 'H'.

  w_tp_arrec_12-data_pagamento_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_12-autoriza_pagamento = 'S'.
  w_tp_arrec_12-valor_principal = <f_zimp_detalhe>-vlr_principal * 100.
  w_tp_arrec_12-valor_juros = <f_zimp_detalhe>-vlr_juros.
  w_tp_arrec_12-valor_multa = <f_zimp_detalhe>-vlr_multa.
  w_tp_arrec_12-valor_at_monet = <f_zimp_detalhe>-vlr_atual_mone.
  w_tp_arrec_12-cod_barras = <f_zimp_detalhe>-cod_barras.
  w_tp_arrec_12-data_vencimento = <f_zimp_cabecalho>-zfbdt.

  PERFORM zf_banco_agencia USING w_tp_arrec_12-agencia
                                 w_tp_arrec_12-conta.

*  w_tp_arrec_12-cr_lf = cl_abap_char_utilities=>cr_lf.
*Início Alteração Ricardo Furst 07.06.2009
*  w_tp_arrec_12-cr_lf = '..'.
  w_tp_arrec_12-reserva_3 = '                                                                    .'.
*Fim Alteração Ricardo Furst 07.06.2009

  w_arquivo = w_tp_arrec_12.

ENDFORM.                    " ZF_MONTA_TP_12
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_13
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_monta_tp_13 .

  CLEAR w_tp_arrec_13.

  w_tp_arrec_13-ident_tributo = 'G'.

  PERFORM zf_nome_cliente USING  w_tp_arrec_13-num_inscricao.

  w_tp_arrec_13-nome_cliente   = v_butxt.
  w_tp_arrec_13-end_cliente    = w_adrc-street.
  w_tp_arrec_13-cep            = w_adrc-post_code1.
  w_tp_arrec_13-uf_cliente     = w_adrc-region.
  w_tp_arrec_13-cidade_cliente = w_adrc-city1.
  w_tp_arrec_13-tipo_inscricao = '2'.
  w_tp_arrec_13-tel_cliente_pagador = w_adrc-tel_number.
  w_tp_arrec_13-data_pagamento_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_13-autoriza_pagamento = 'S'.
  w_tp_arrec_13-valor_principal = <f_zimp_detalhe>-vlr_principal * 100.
  w_tp_arrec_13-valor_juros = <f_zimp_detalhe>-vlr_juros * 100.
  w_tp_arrec_13-valor_multa = <f_zimp_detalhe>-vlr_multa * 100.
  w_tp_arrec_13-cod_barras = <f_zimp_detalhe>-cod_barras.
  w_tp_arrec_13-data_vencimento_tributo = <f_zimp_cabecalho>-zfbdt.
  w_tp_arrec_13-codigo_receita = <f_zimp_cabecalho>-cod_pgto.
  w_tp_arrec_13-referencia = <f_zimp_cabecalho>-cod_pgto.
  w_tp_arrec_13-ano = <f_zimp_cabecalho>-mes_ano_comp(4).
  w_tp_arrec_13-mes = <f_zimp_cabecalho>-mes_ano_comp+4.

*  w_tp_arrec_13-cr_lf = cl_abap_char_utilities=>cr_lf.
*Início Alteração Ricardo Furst 07.06.2009
*  w_tp_arrec_13-cr_lf = '..'.
  w_tp_arrec_13-reserva_4 = '                                           .'.
*Fim Alteração Ricardo Furst 07.06.2009

  w_arquivo = w_tp_arrec_13.

ENDFORM.                    " ZF_MONTA_TP_13
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_grava_arquivo .

  DATA:
    wl_tabname(15),
    wl_index(2).
  FIELD-SYMBOLS <fs_tab> TYPE ANY TABLE.

  DO 13 TIMES.
    MOVE sy-index TO wl_index.
*  CASE SY-INDEX.
    CONCATENATE 't_arquivo' wl_index '[]' INTO wl_tabname.
    ASSIGN (wl_tabname) TO <fs_tab>.
    IF sy-subrc = 0.
      IF NOT <fs_tab> IS INITIAL.
* Início Alteração Ricardo Furst 13.07.2009
*        IF wl_index NE '8'.
*          CONCATENATE  p_path
*          <f_zimp_cabecalho>-nro_doc_tr '_' sy-datum '_tp_' wl_index '.txt'
*          INTO lv_nome_arquivo.
*        ELSE.
*          CONCATENATE  p_path
*        '0000000000' '_' sy-datum '_tp_' wl_index '.txt'
*        INTO lv_nome_arquivo.
*        ENDIF.
        MOVE p_path TO lv_nome_arquivo.
        MOVE <fs_tab>        TO t_arquivo[].
        v_arqs_proc = v_arqs_proc + 1.
        IF p_pc IS INITIAL.
          OPEN DATASET lv_nome_arquivo FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

          IF sy-subrc <> 0.
            MESSAGE 'Caminho ou nome de arquivo inválido. Impossível continuar!'
            TYPE 'A'.
          ENDIF.

          LOOP AT t_arquivo INTO w_arquivo.
            TRANSFER w_arquivo TO lv_nome_arquivo.
          ENDLOOP.

          CLOSE DATASET lv_nome_arquivo.
        ELSE.
          DATA: wl_filename TYPE rlgrap-filename.
          MOVE: lv_nome_arquivo TO wl_filename.

          CALL FUNCTION 'WS_DOWNLOAD'
            EXPORTING
              filename                = wl_filename
            TABLES
              data_tab                = t_arquivo
            EXCEPTIONS
              file_open_error         = 1
              file_write_error        = 2
              invalid_filesize        = 3
              invalid_type            = 4
              no_batch                = 5
              unknown_error           = 6
              invalid_table_width     = 7
              gui_refuse_filetransfer = 8
              customer_error          = 9
              no_authority            = 10
              OTHERS                  = 11.
          IF sy-subrc <> 0.
            MESSAGE ID '00' TYPE 'E' NUMBER '398' WITH
               'Erro ao criar o arquivo'
               <f_zimp_cabecalho>-nro_doc_tr
               'na pasta'
               p_path.
          ENDIF.


        ENDIF.


      ENDIF.
    ENDIF.
  ENDDO.

* Início Alteração Ricardo Furst 07.07.2009
  PERFORM gera_relatorio.
* Fim Alteração Ricardo Furst 07.07.2009

** Gerando arquivo no servidor (pasta c:\testando)
*  CONCATENATE  P_PATH "'c:\testando\'
*              <f_zimp_cabecalho>-nro_doc_tr '_' sy-datum '.txt'
*              INTO lv_nome_arquivo.
*   IF P_PC IS INITIAL.
*     OPEN DATASET lv_nome_arquivo FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*
*     IF sy-subrc <> 0.
*       MESSAGE 'Caminho ou nome de arquivo inválido. Impossível continuar!'
*       TYPE 'A'.
*     ENDIF.
*
*     LOOP AT t_arquivo INTO w_arquivo.
*       TRANSFER w_arquivo TO lv_nome_arquivo.
*     ENDLOOP.
*
*     CLOSE DATASET lv_nome_arquivo.
*   ELSE.
*     DATA: wl_filename type RLGRAP-FILENAME.
*     move lv_nome_arquivo to wl_filename.
*     CALL FUNCTION 'WS_DOWNLOAD'
*      EXPORTING
*        FILENAME                      = wl_filename
*       TABLES
*         data_tab                      = t_arquivo
*      EXCEPTIONS
*        FILE_OPEN_ERROR               = 1
*        FILE_WRITE_ERROR              = 2
*        INVALID_FILESIZE              = 3
*        INVALID_TYPE                  = 4
*        NO_BATCH                      = 5
*        UNKNOWN_ERROR                 = 6
*        INVALID_TABLE_WIDTH           = 7
*        GUI_REFUSE_FILETRANSFER       = 8
*        CUSTOMER_ERROR                = 9
*        NO_AUTHORITY                  = 10
*        OTHERS                        = 11
*               .
*     IF sy-subrc <> 0.
*       Message id '00' TYPE 'E' NUMBER '398' with
*          'Erro ao criar o arquivo'
*          <f_zimp_cabecalho>-nro_doc_tr
*          'na pasta'
*          p_path.
*     ENDIF.
*
*
*   ENDIF.
ENDFORM.                    " ZF_GRAVA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  ZF_INFORMACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_informacao .

  MESSAGE i398(00) WITH v_regs_proc
                        'registros foram processados em'
                        v_arqs_proc 'arquivo(s).'.

ENDFORM.                    " ZF_INFORMACAO
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_8
*&---------------------------------------------------------------------*
FORM zf_monta_tp_8 .

  DATA: BEGIN OF t_contas OCCURS 0.
          INCLUDE STRUCTURE zimp_contas_cons.
  DATA: venci LIKE sy-datum.
  DATA: END OF t_contas.

  SELECT * FROM zimp_contas_cons
    INTO TABLE t_contas
    WHERE bukrs        EQ p_emp    AND
            belnr      IN s_nr_doc AND
            budat      IN s_dtlanc AND
            lifnr      IN s_lifnr  AND
            doc_comp   EQ space    and
            cod_barras ne space.
*  AND
*            zfbdt      IN s_dtvenc." AND
*            tp_arrec   IN s_tparrc AND
*            belnr      <> space    AND
*            arq_gerado  = space.


  LOOP AT t_contas.
    t_contas-venci = t_contas-zfbdt + t_contas-zbd1t.
    MODIFY t_contas.
  ENDLOOP.

  DELETE t_contas WHERE NOT venci IN s_dtvenc.

  LOOP AT t_contas.
    v_tot_tit = v_tot_tit + t_contas-dmbtr.

    CLEAR w_tp_arrec_8_9_10_11.

*  DATA: w_valor(15) TYPE n.


    w_tp_arrec_8_9_10_11-ident_tributo = 'B'.

    w_tp_arrec_8_9_10_11-cod_barras = t_contas-cod_barras.
    w_tp_arrec_8_9_10_11-data_pagamento_tributo = t_contas-venci.

    w_tp_arrec_8_9_10_11-autoriza_pagamento = 'S'.





    PERFORM zf_banco_agencia USING w_tp_arrec_8_9_10_11-agencia
                                   w_tp_arrec_8_9_10_11-conta.

*  w_tp_arrec_8_9_10_11-cr_lf = cl_abap_char_utilities=>cr_lf.
*Início Alteração Ricardo Furst 07.06.2009
*    w_tp_arrec_8_9_10_11-cr_lf = '..'.
*Fim Alteração Ricardo Furst 07.06.2009

    w_arquivo = w_tp_arrec_8_9_10_11.
    w_arquivo+462 = '.'.

    APPEND w_arquivo TO t_arquivo8.
    APPEND w_arquivo TO t_arquivo.

    v_regs_proc = v_regs_proc + 1.


*    CONCATENATE t_contas-budat+6(2)
*                t_contas-budat+4(2)
*                t_contas-budat(4)
*           INTO t_batch-bldat.

*BREAK-POINT.
    t_contas-zfbdt = t_contas-zfbdt + t_contas-zbd1t.

    CONCATENATE t_contas-zfbdt+6(2)
                t_contas-zfbdt+4(2)
                t_contas-zfbdt(4)
           INTO t_batch-budat.

    t_batch-blart = 'ZP'.
    t_batch-bukrs = t_contas-bukrs.
    CONCATENATE t_contas-budat+6(2)
                t_contas-budat+4(2)
                t_contas-budat(4)
           INTO t_batch-bldat.
    t_batch-waers = 'BRL'.

    SELECT *
      INTO w_t012k
      FROM t012k
      UP TO 1 ROWS
      WHERE bukrs = t_contas-bukrs
        AND hbkid = p_bncemp.
    ENDSELECT.

    t_batch-konto = w_t012k-hkont.
    CONCATENATE t_contas-bukrs+2(2)
                '01'
           INTO t_batch-gsber.
*    t_batch-gsber = '0101'."t_contas-gsber.
    t_batch-wrbtr = t_contas-dmbtr.
    REPLACE '.' WITH ',' INTO t_batch-wrbtr.
    t_batch-sel01 = t_contas-belnr.
    t_batch-agkon = t_contas-lifnr.

    MOVE: t_batch-budat TO t_batch-data,
          t_contas-belnr TO t_batch-belnr,
          t_contas-gjahr TO t_batch-gjahr.

    APPEND t_batch.

  ENDLOOP.

ENDFORM.                    " ZF_MONTA_TP_8
*&---------------------------------------------------------------------*
*&      Form  MATCH_CODE_TPARRC
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
    INTO TABLE t_zimp_tipos_impos.

  SORT t_zimp_tipos_impos BY tp_arrec.

  LOOP AT t_zimp_tipos_impos.

    t_value-line = t_zimp_tipos_impos-tp_arrec.
    APPEND t_value.

    t_value-line = t_zimp_tipos_impos-arrecadacao.
    APPEND t_value.

  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
    EXPORTING
      titel                     = text-001
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
*&      Form  GERA_RELATORIO
*&---------------------------------------------------------------------*
FORM gera_relatorio .

  WRITE:/ sy-uline.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.


  READ TABLE s_dtvenc INDEX 1.
  WRITE:/1 sy-vline,
         2 'Empresa Pagadora:',
         20 p_emp," P_bukrs,
         202 sy-vline.

  WRITE:/1 sy-vline,
         2 'Nome do Arquivo:',
         19 lv_nome_arquivo,
         202 sy-vline.

  WRITE:/1 sy-vline,
         2 'Data Vencimento:',
*         19 <f_zimp_cabecalho>-zfbdt,
         19 s_dtvenc-low,
         202 sy-vline.

  WRITE:/1 sy-vline,
         2 'Valor Total dos Títulos:',
         27 v_tot_tit,
         202 sy-vline.

  WRITE:/1 sy-uline.

ENDFORM.                    " GERA_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  ZF_BATCH_INPUT
*&---------------------------------------------------------------------*
FORM zf_batch_input .

*  PERFORM f_tbatch.

  LOOP AT t_batch.
* executa o mapeamento
    PERFORM f_mapeamento.
* executa a transação
    PERFORM f_transaction.
* trata msg
    PERFORM f_log.

  ENDLOOP.

ENDFORM.                    " ZF_BATCH_INPUT
*&---------------------------------------------------------------------*
*&      Form  F_TBATCH
*&---------------------------------------------------------------------*
FORM f_tbatch.

  CONCATENATE <f_zimp_cabecalho>-bldat+6(2)
              <f_zimp_cabecalho>-bldat+4(2)
              <f_zimp_cabecalho>-bldat(4)
         INTO t_batch-bldat.
  t_batch-blart = 'ZP'.
  t_batch-bukrs = <f_zimp_cabecalho>-bukrs.
  CONCATENATE <f_zimp_cabecalho>-budat+6(2)
              <f_zimp_cabecalho>-budat+4(2)
              <f_zimp_cabecalho>-budat(4)
         INTO t_batch-budat.
  t_batch-waers = 'BRL'.

  SELECT *
    INTO w_t012k
    FROM t012k
    UP TO 1 ROWS
    WHERE bukrs = <f_zimp_cabecalho>-bukrs
      AND hbkid = p_bncemp.
  ENDSELECT.

  t_batch-konto = w_t012k-hkont.
  t_batch-gsber = <f_zimp_detalhe>-gsber.
  t_batch-wrbtr = v_tit.
  REPLACE '.' WITH ',' INTO t_batch-wrbtr.
  t_batch-sel01 = <f_zimp_cabecalho>-belnr.
  t_batch-agkon = <f_zimp_cabecalho>-lifnr.
  APPEND t_batch.

ENDFORM.                    " F_TBATCH

*&---------------------------------------------------------------------*
*&      Form  f_mapeamento
*&---------------------------------------------------------------------*
FORM f_mapeamento .
*eduardo
*if v_budat is INITIAL.
  v_budat = t_batch-budat.
*  endif.

  CLEAR ti_bdc[].

  PERFORM f_shdb USING:

'SAPMF05A'  '0103'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '/00'                 ,
   ''         ''    ''                                  'BKPF-BLDAT'  t_batch-bldat         ,
   ''         ''    ''                                  'BKPF-BLART'  'ZP'                  ,
   ''         ''    ''                                  'BKPF-BUKRS'  t_batch-bukrs         ,
* Inclusão - Inicio - KZORZAN - ROLLOUT - 30.07.2009 ******************
*   ''         ''    ''                                  'BKPF-BUDAT'  <f_zimp_cabecalho>-zfbdt, " t_batch-budat         ,
   ''         ''    ''                                  'BKPF-BUDAT'   t_batch-data         ,
* Inclusão - Inicio - KZORZAN - ROLLOUT - 30.07.2009 ******************
   ''         ''    ''                                  'BKPF-MONAT'  '7'                   ,
   ''         ''    ''                                  'BKPF-WAERS'  'BRL'                 ,
   ''         ''    ''                                  'RF05A-KONTO' t_batch-konto         ,
   ''         ''    ''                                  'BSEG-GSBER'  t_batch-gsber         ,
   ''         ''    ''                                  'BSEG-WRBTR'  t_batch-wrbtr         ,
   ''         ''    ''                                  'RF05A-AGKON' t_batch-agkon         ,
   ''         ''    ''                                  'RF05A-AGKOA' 'K'                   ,
   ''         ''    ''                                  'RF05A-XNOPS' 'X'                   ,
   ''         ''    ''                                  'RF05A-XPOS1(01)' ''                ,
   ''         ''    ''                                  'RF05A-XPOS1(03)' 'X'               ,

'SAPMF05A'  '0731'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '=PA'                 ,
   ''         ''    ''                                  'RF05A-SEL01(01)' t_batch-sel01    ,

'SAPDF05X'  '3100'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '=PI'                 ,
   ''         ''    ''                                  'BDC_SUBSCR'  'SAPDF05X                                6102PAGE',
   ''         ''    ''                                  'RF05A-ABPOS' '1'                   ,

'SAPDF05X'  '3100'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '=BS'                 ,
   ''         ''    ''                                  'BDC_SUBSCR'   'SAPDF05X                                6102PAGE',
   ''         ''    ''                                  'RF05A-ABPOS'  '1'                  ,

'SAPMF05A'  '0700' 'X'                                      ''          ''                  ,
   ''         ''    ''                                  'BDC_OKCODE'  '=BU'                 .

ENDFORM.                    " f_mapeamento
*&---------------------------------------------------------------------*
*&      Form  f_transaction
*&---------------------------------------------------------------------*
FORM f_transaction .

  v_mode = 'E'.

  CLEAR ti_msg[].
  CALL TRANSACTION 'F-53'
        USING ti_bdc
        MODE v_mode
        UPDATE 'S'
        MESSAGES INTO ti_msg.

  IF sy-subrc = 0.
    UPDATE zimp_contas_cons SET doc_comp = 'X'
                          WHERE bukrs = t_batch-bukrs AND
                                belnr = t_batch-belnr AND
                                gjahr = t_batch-gjahr.
  ENDIF.

ENDFORM.                    " f_transaction
*&---------------------------------------------------------------------*
*&      Form  f_log
*&---------------------------------------------------------------------*
FORM f_log .

  CLEAR wa_msg.

  wa_log-bldat = t_batch-bldat.
  wa_log-blart = t_batch-blart.
  wa_log-bukrs = t_batch-bukrs.
  wa_log-budat = t_batch-budat.
  wa_log-waers = t_batch-waers.
  wa_log-konto = t_batch-konto.
  wa_log-hbkid = t_batch-hbkid.
  wa_log-gsber = t_batch-gsber.
  wa_log-wrbtr = t_batch-wrbtr.
  wa_log-sel01 = t_batch-sel01.

  LOOP AT ti_msg INTO wa_msg.

    v_msgv1 = wa_msg-msgv1.
    v_msgv2 = wa_msg-msgv2.
    v_msgv3 = wa_msg-msgv3.
    v_msgv4 = wa_msg-msgv4.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = wa_msg-msgspra
        msg_id                 = wa_msg-msgid
        msg_no                 = wa_msg-msgnr
        msg_var1               = v_msgv1
        msg_var2               = v_msgv2
        msg_var3               = v_msgv3
        msg_var4               = v_msgv4
      IMPORTING
        msg_text               = v_msg
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      CLEAR wa_log-tipo.
      v_msg = 'Não houve mensagem.'.
    ENDIF.

    wa_log-tipo     = wa_msg-msgtyp.
    wa_log-msg      = v_msg.

    APPEND wa_log TO ti_log.

  ENDLOOP.

ENDFORM.                    " f_logc
*&---------------------------------------------------------------------*
*&      Form  f_shdb
*&---------------------------------------------------------------------*
FORM f_shdb  USING    p_programa
                      p_tela
                      p_id
                      p_campo
                      p_valor.

  CLEAR wa_bdc.

  wa_bdc-program        = p_programa.
  wa_bdc-dynpro         = p_tela.
  wa_bdc-dynbegin       = p_id.
  wa_bdc-fnam           = p_campo.
  wa_bdc-fval           = p_valor.

  APPEND wa_bdc TO ti_bdc.

ENDFORM.                    " f_shdb
