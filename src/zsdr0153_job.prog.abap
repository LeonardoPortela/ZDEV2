*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 22.05.2023                                              &*
*& Descrição: Processamento Gerar Lotes de vendas                     &*
*&--------------------------------------------------------------------&*
*& Projeto  : Algodão                                                 &*
*&--------------------------------------------------------------------&*
REPORT zsdr0153_job.

************************************************************************
* tabelas
************************************************************************
TABLES: j_1bbranch, j_1bnflin, zsdt0001, j_1bnfe_active, zmmt0126, zsdt0330.

************************************************************************
*& constantes
************************************************************************
CONSTANTS: c_s TYPE c VALUE 'S',
           c_0 TYPE c VALUE '0',
           c_1 TYPE c VALUE '1',
           c_x TYPE c VALUE 'X'.

************************************************************************
*& estruturas
************************************************************************
TYPES: BEGIN OF ty_carga,
         ch_referencia TYPE zsdt0330-ch_referencia,
         nr_romaneio   TYPE zmmt0008-nr_romaneio,
         werks         TYPE zsdt0330-werks,
         lgort         TYPE zsdt0330-lgort,
         safra         TYPE zsdt0330-safra,
         kunnr         TYPE zsdt0330-kunnr,
         placa_cav     TYPE zsdt0330-placa_cav,
         motorista     TYPE char100.
TYPES: END OF ty_carga.

TYPES: BEGIN OF ty_zsdt0001,
         nr_romaneio TYPE zsdt0001-nr_romaneio,
         vbeln       TYPE zsdt0001-vbeln,
         branch      TYPE zsdt0001-branch,
         doc_rem     TYPE zsdt0001-doc_rem,
         id_cli_dest TYPE zsdt0001-id_cli_dest,
       END OF ty_zsdt0001,

       BEGIN OF ty_vbrp,
         vbeln  TYPE vbrp-vbeln,
         vgbel  TYPE vbrp-vgbel,
         matnr  TYPE vbrp-matnr,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_vbrp,

       BEGIN OF ty_lin,
         docnum TYPE j_1bnflin-docnum,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_lin,

       BEGIN OF ty_bnfe,
         docnum TYPE j_1bnfe_active-docnum,
         nfnum9 TYPE j_1bnfe_active-nfnum9,
         bukrs  TYPE j_1bnfe_active-bukrs,
         branch TYPE j_1bnfe_active-branch,
       END OF ty_bnfe,

       BEGIN OF ty_zmmt0008,
         werks              TYPE zmmt0008-werks,
         lgort              TYPE zmmt0008-lgort,
         nr_romaneio        TYPE zmmt0008-nr_romaneio,
         charg              TYPE zmmt0008-charg,
         menge              TYPE zmmt0008-menge,
         werks_orig         TYPE zmmt0008-werks_orig,
         lgortr             TYPE lgort_d,  "*-S4H-US 123905-25.09.2023-JT
         safra              TYPE charg_d,  "*-S4H-US 123905-25.09.2023-JT
         mblnr              TYPE mblnr,    "*-S4H-US 123905-25.09.2023-JT
         mjahr              TYPE mjahr,    "*-S4H-US 123905-25.09.2023-JT
         dados_contingencia TYPE char01,   "*-S4H-US 123905-25.09.2023-JT
       END OF ty_zmmt0008,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         normt TYPE mara-normt,
       END OF ty_mara,

       BEGIN OF ty_total,
         total TYPE sy-tabix,
         tipo  TYPE mara-normt,
         peso  TYPE zmmt0008-menge,
       END OF ty_total.

TYPES: BEGIN OF ty_fardos_exc,
         fardos  TYPE char21,
         peso    TYPE zmms004-peso,
         tipo    TYPE zmms004-tipo,
         tamanho TYPE zmmt0008-tipo_fardo,
       END OF ty_fardos_exc.

TYPES: BEGIN OF ty_zsdt0045,
         zseq_inst     TYPE zsdt0045-zseq_inst,
         objek         TYPE zsdt0045-objek,
         objecttable   TYPE zsdt0045-objecttable,
         booking       TYPE zsdt0045-booking,
         instrucao     TYPE zsdt0045-instrucao,
         tamanho_fardo TYPE zsdt0045-tamanho_fardo,
       END OF ty_zsdt0045.

TYPES: BEGIN OF ty_excel,
         empresa    TYPE zmms005-empresa,
         safra      TYPE zmms005-safra,
         remetente  TYPE zmms005-remetente,
         terminal   TYPE zmms005-destinatario,
         cliente    TYPE zmms005-cliente,
         romaneio   TYPE zmms005-romaneio,
         nfnum      TYPE zmms005-nfnum,
         motorista  TYPE zmms005-motorista,
         veiculo    TYPE zmms005-veiculo,
         bloco      TYPE zmms005-bloco,
         qtd_fardos TYPE zmms006-total,
         peso_total TYPE zmms004-peso,
         instrucao  TYPE zsdt0066-instrucao,
         stcd1      TYPE j_1bbranch-stcd1,
       END OF ty_excel.

TYPES: BEGIN OF ty_saida,
         mark               TYPE c,
         werks              TYPE zmmt0008-werks,
         bloco              TYPE zmmt0008-lgort,
         fardo              TYPE zmmt0008-charg,
         nr_romaneio        TYPE zmmt0008-nr_romaneio,
         nfnum              TYPE zmmt0008-nfnum,
         vbeln              TYPE zmmt0008-vbeln,
         vbeln_vf           TYPE zmmt0008-vbeln_vf,
         placa_cav          TYPE zmmt0008-placa_cav,
         matnr              TYPE vbrp-matnr,
         menge              TYPE zmmt0008-menge,
         werks_orig         TYPE zmmt0008-werks_orig,
         ch_ref             TYPE zsdt0330-ch_referencia,
         lgortr             TYPE lgort_d,  "*-S4H-US 123905-25.09.2023-JT
         safra              TYPE charg_d,  "*-S4H-US 123905-25.09.2023-JT
         mblnr              TYPE mblnr,    "*-S4H-US 123905-25.09.2023-JT
         mjahr              TYPE mjahr,    "*-S4H-US 123905-25.09.2023-JT
         dados_contingencia TYPE char01,   "*-S4H-US 123905-25.09.2023-JT
       END OF ty_saida.

************************************************************************
*& variaveis globais
************************************************************************
DATA: t_zsdt0330      TYPE TABLE OF zsdt0330,
      w_zsdt0330      TYPE zsdt0330,
      t_zsdt0330_grp  TYPE TABLE OF zsdt0330,
      t_carga         TYPE TABLE OF ty_carga,
      t_lote          TYPE TABLE OF zsdt0330,
      w_lote          TYPE zsdt0330,
      t_status        TYPE zde_btcstatus_t,
      l_tabix         TYPE sy-tabix,
      l_mensagem      TYPE string,
      l_jobs          TYPE i,
      l_erro          TYPE char01,
      w_zsdt0330_grp  TYPE zsdt0330,
      it_zmail        TYPE STANDARD TABLE OF zmail,
      wa_zmail        TYPE zmail,
*
      w_carga         TYPE ty_carga,
      tg_zsdt0001     TYPE TABLE OF ty_zsdt0001,
      wg_zsdt0001     TYPE ty_zsdt0001,
      tg_vbrp         TYPE TABLE OF ty_vbrp,
      wg_vbrp         TYPE ty_vbrp,
      tg_lin          TYPE TABLE OF ty_lin,
      wg_lin          TYPE ty_lin,
      tg_bnfe         TYPE TABLE OF ty_bnfe,
      wg_bnfe         TYPE ty_bnfe,
      tg_saida        TYPE TABLE OF ty_saida,
      wg_saida        TYPE ty_saida,
      tg_zmmt0008     TYPE TABLE OF ty_zmmt0008,
      tg_zmmt0008_aux TYPE TABLE OF ty_zmmt0008,
      wg_zmmt0008     TYPE ty_zmmt0008,
      tg_kna1         TYPE TABLE OF ty_kna1,
      wg_kna1         TYPE ty_kna1,
      wg_header       TYPE zmms005,
      tg_fardos       TYPE TABLE OF zmms004 WITH HEADER LINE,
      tg_mara         TYPE TABLE OF ty_mara,
      wg_mara         TYPE ty_mara,
      tg_excel        TYPE TABLE OF ty_excel,
      wg_excel        TYPE ty_excel,
      wa_zsdt0045     TYPE ty_zsdt0045,
      tg_fardos_exc   TYPE TABLE OF ty_fardos_exc,
      wg_fardos_exc   TYPE ty_fardos_exc,
      v_name(30),
      v_input,
      v_required,
      v_request,
      v_value,
      v_txtbtn        TYPE c LENGTH 50,
      v_init          TYPE c,
      l_mesg1         TYPE char200,
      l_mesg2         TYPE char200,
      l_mesg3         TYPE char200,
*
      send_request    TYPE REF TO cl_bcs,
      document        TYPE REF TO cl_document_bcs,
      recipient       TYPE REF TO if_recipient_bcs,
      bcs_exception   TYPE REF TO cx_bcs,
*
      main_text       TYPE bcsy_text,
      binary_content  TYPE solix_tab,
      size            TYPE so_obj_len,
      sent_to_all     TYPE os_boolean.

************************************************************************
*  parametro ID_CARGA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_carga   FOR zsdt0330-id_carga.
  PARAMETERS     : p_unico  AS CHECKBOX,
                   p_idcar  LIKE zsdt0330-id_carga,
                   p_matnr  LIKE zsdt0330-matnr,
                   p_werks  LIKE zsdt0330-werks,
                   p_lgort  LIKE zsdt0330-lgort,
                   p_acharg LIKE zsdt0330-acharg,
                   p_safra  LIKE zsdt0330-safra,
                   p_repro  AS CHECKBOX,
                   p_email  AS CHECKBOX,
                   p_integ  AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK b1.

************************************************************************
*&      Start-Of-Selection
************************************************************************
START-OF-SELECTION.

  FREE: t_status.

  APPEND 'R' TO t_status.

*---------------------------------------------
* se tem Job ativo, abandona
*---------------------------------------------
  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status    " Status de Jobs
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd > 1.
      EXIT.
    ENDIF.
  ENDIF.

*---------------------------------------------
*-Processamento
*---------------------------------------------
  PERFORM f_selecao_fardos.

************************************************************************
*&-selecao dos fardos
************************************************************************
FORM f_selecao_fardos.

  RANGES: lra_data_carga FOR zsdt0330-data_carga.
  DATA: lva_dt_read_carga TYPE zsdt0330-data_carga.

  SELECT SINGLE * FROM tvarvc INTO @DATA(lwa_zsdr0153_job) WHERE name = 'ZSDR0153_JOB_DAYS_READ'.
  IF sy-subrc EQ 0.
    lva_dt_read_carga = sy-datum - lwa_zsdr0153_job-low.

    APPEND VALUE #( sign = 'I' option = 'GE' low = lva_dt_read_carga ) TO lra_data_carga.
  ENDIF.

  FREE: t_carga.

*------------------------------------------
*- recupera ch_referencia
*------------------------------------------
  SELECT *
    FROM zsdt0330
    INTO TABLE t_zsdt0330
   WHERE id_carga         IN s_carga
     AND status_fardo      =  '3'
     AND status_gera_lote IN ('0','2')
     AND status_estorno   IN (abap_false,'I')
*    AND ch_referencia     = abap_off
     AND cancelado         = abap_false
     AND data_carga      IN lra_data_carga.

  LOOP AT t_zsdt0330 INTO w_zsdt0330.
    SELECT SINGLE  ch_referencia
      INTO @DATA(l_ch_referencia)
      FROM zsdt0001
     WHERE tp_movimento = 'S'
       AND nr_romaneio  = @w_zsdt0330-nr_romaneio
       AND vbeln        = @w_zsdt0330-vbeln.

    CHECK sy-subrc = 0.

    CHECK w_zsdt0330-ch_referencia NE l_ch_referencia.

    "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
    "w_zsdt0330-ch_referencia = l_ch_referencia.
    "MODIFY zsdt0330       FROM w_zsdt0330.
    UPDATE zsdt0330 SET ch_referencia = l_ch_referencia
                        WHERE id_carga      EQ w_zsdt0330-id_carga
                          AND matnr         EQ w_zsdt0330-matnr
                          AND werks         EQ w_zsdt0330-werks
                          AND lgort         EQ w_zsdt0330-lgort
                          AND acharg        EQ w_zsdt0330-acharg
                          AND safra         EQ w_zsdt0330-safra
                          AND seq           EQ w_zsdt0330-seq.
    "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---
  ENDLOOP.

  COMMIT WORK.

*------------------------------------------
*- selecao dos fardos a processar
*------------------------------------------
  IF p_unico = abap_true.
    SELECT *
      FROM zsdt0330
      INTO TABLE t_zsdt0330
     WHERE id_carga          = p_idcar
       AND matnr             = p_matnr
       AND werks             = p_werks
       AND lgort             = p_lgort
       AND acharg            = p_acharg
       AND safra             = p_safra
       AND status_fardo      =  '3'
       AND status_gera_lote IN ('0','2','3')
       AND status_estorno   IN (abap_false,'I')
       AND cancelado         = abap_false.
  ELSE.
    SELECT *
      FROM zsdt0330
      INTO TABLE t_zsdt0330
     WHERE id_carga         IN s_carga
       AND status_fardo      =  '3'
       AND status_gera_lote IN ('0','2')
       AND status_estorno   IN (abap_false,'I')
       AND cancelado         = abap_false
       AND data_carga       IN lra_data_carga.
  ENDIF.

  CHECK t_zsdt0330[] IS NOT INITIAL.

  t_zsdt0330_grp[] = t_zsdt0330[].

*---------------------------------
* altera status processamento
*---------------------------------
  LOOP AT t_zsdt0330         INTO w_zsdt0330.
    w_zsdt0330-status_gera_lote = '1'.


    "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
    "MODIFY zsdt0330          FROM w_zsdt0330.
    UPDATE zsdt0330 set status_gera_lote = w_zsdt0330-status_gera_lote
                        WHERE id_carga      EQ w_zsdt0330-id_carga
                          AND matnr         EQ w_zsdt0330-matnr
                          AND werks         EQ w_zsdt0330-werks
                          AND lgort         EQ w_zsdt0330-lgort
                          AND acharg        EQ w_zsdt0330-acharg
                          AND safra         EQ w_zsdt0330-safra
                          AND seq           EQ w_zsdt0330-seq.
    "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---

    COMMIT WORK AND WAIT.
  ENDLOOP.

*---------------------------------
* agrupa por centro + deposito + safra
*---------------------------------
  SORT t_zsdt0330_grp BY ch_referencia safra werks lgort.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0330_grp
                        COMPARING ch_referencia safra werks lgort.

*---------------------------------
* processamento
*---------------------------------
  LOOP AT t_zsdt0330_grp INTO w_zsdt0330_grp.
    FREE: t_carga, t_lote.

    MOVE-CORRESPONDING w_zsdt0330_grp TO w_carga.

    SELECT SINGLE nr_romaneio
      FROM zsdt0001
      INTO @DATA(_nr_romaneio)
     WHERE tp_movimento  = @c_s
       AND ch_referencia = @w_zsdt0330_grp-ch_referencia
       AND branch        = @w_zsdt0330_grp-werks
       AND nr_safra      = @w_zsdt0330_grp-safra.

    IF sy-subrc <> 0.
      l_mensagem   = 'Romaneio informado não foi encontrado'.
      PERFORM f_gera_erro_carga USING '2' l_mensagem.
      CONTINUE.
    ENDIF.

    MOVE _nr_romaneio                 TO w_carga-nr_romaneio.

*-----------------------------
* -lotes da carga
*-----------------------------
    LOOP AT t_zsdt0330 INTO w_zsdt0330 WHERE ch_referencia = w_zsdt0330_grp-ch_referencia
                                         AND safra         = w_zsdt0330_grp-safra
                                         AND werks         = w_zsdt0330_grp-werks
                                         AND lgort         = w_zsdt0330_grp-lgort.
      APPEND w_zsdt0330  TO t_lote.
    ENDLOOP.

    APPEND w_carga                    TO t_carga.

    PERFORM f_carrega_outros_dados.

    "Projeto Restruturação Algodao Fase 2 - WPP 29/07/24 - Ini
    PERFORM f_gerar_espelho_romaneio.

*    PERFORM f_selecao_dados CHANGING l_erro.
*
*    CHECK l_erro = abap_off.
*
*    PERFORM f_organiza_dados.
*    PERFORM f_atualiza_zmmt0008.
    "Projeto Restruturação Algodao Fase 2 - WPP 29/07/24 - Fim

  ENDLOOP.

ENDFORM.

FORM f_gerar_espelho_romaneio.

  DATA: lit_fardos_espelho TYPE zsds091_t.

  CLEAR: lit_fardos_espelho[], tg_saida[].

  LOOP AT t_lote INTO DATA(lwa_fardo_carga).

    APPEND VALUE #( werks = lwa_fardo_carga-werks
                    bloco = lwa_fardo_carga-lgort
                    fardo = lwa_fardo_carga-acharg
                    safra = lwa_fardo_carga-safra
                  ) TO lit_fardos_espelho.

    APPEND VALUE #( ch_ref = w_carga-ch_referencia
                    mark  = abap_true
                    werks = lwa_fardo_carga-werks
                    bloco = lwa_fardo_carga-lgort
                    fardo = lwa_fardo_carga-acharg
                    safra = lwa_fardo_carga-safra
                  ) TO tg_saida.
  ENDLOOP.

  zcl_comercializacao_algodao=>gerar_espelho_romaneio(
   EXPORTING
     i_werks             = CONV #( w_carga-werks )
     i_lgort             = CONV #( w_carga-lgort )
     i_safra             = CONV #( w_carga-safra )
     i_nr_romaneio       = CONV #( w_carga-nr_romaneio )
     i_motorista         = CONV #( w_carga-motorista )
     i_placa             = CONV #( w_carga-placa_cav )
     i_kunnr             = CONV #( w_carga-kunnr )
     i_fardos_espelho    = lit_fardos_espelho
     i_proc_unico        = p_unico
     i_reprocessar       = p_repro
     i_envia_email       = p_email
     i_integra_rondoline = p_integ
     i_carregamento_auto = abap_true
   IMPORTING
     e_msg_error         = DATA(lva_msg_error)
     e_msg_sucesso       = DATA(lva_msg_sucesso) ).

  IF lva_msg_error IS NOT INITIAL.
    PERFORM f_gera_erro_carga USING '2' lva_msg_error.
  ELSEIF lva_msg_sucesso IS NOT INITIAL.

    IF p_unico = abap_false OR p_repro = abap_true.
      PERFORM f_monta_log     USING '3'
                                    abap_off
                                    lva_msg_sucesso.
    ELSE.
      PERFORM  f_atual_status USING '3'.
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.

************************************************************************
*&-gerar erro para o fardo
************************************************************************
FORM f_gera_erro_carga USING p_status
                             p_mensagem.

  LOOP AT t_zsdt0330 INTO w_zsdt0330 WHERE ch_referencia = w_carga-ch_referencia
                                       AND werks         = w_carga-werks
                                       AND lgort         = w_carga-lgort
                                       AND safra         = w_carga-safra.

    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_log( EXPORTING i_id_carga     = w_zsdt0330-id_carga
                                    i_matnr        = w_zsdt0330-matnr
                                    i_werks        = w_zsdt0330-werks
                                    i_lgort        = w_zsdt0330-lgort
                                    i_acharg       = w_zsdt0330-acharg
                                    i_safra        = w_zsdt0330-safra
                                    i_tipo_integra = 'GL'
                                    i_tipo_msg     = 'E'
                                    i_mensagem     = p_mensagem ).

    UPDATE zsdt0330 SET status_gera_lote = p_status
                  WHERE id_carga         = w_zsdt0330-id_carga
                    AND matnr            = w_zsdt0330-matnr
                    AND werks            = w_zsdt0330-werks
                    AND lgort            = w_zsdt0330-lgort
                    AND acharg           = w_zsdt0330-acharg
                    AND safra            = w_zsdt0330-safra
                    AND cancelado        = abap_false.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

************************************************************************
*&-carregar outras informacoes
************************************************************************
FORM f_carrega_outros_dados.

  READ TABLE t_carga INTO w_carga INDEX 1.

  SELECT vbeln, id_cli_dest, placa_cav, motorista
    INTO TABLE @DATA(t_0001)
    FROM zsdt0001
   WHERE ch_referencia = @w_carga-ch_referencia.
*    AND nr_romaneio   = @w_carga-nr_romaneio
*    AND nr_safra      = @w_carga-safra
*    AND branch        = @w_carga-werks.

  IF sy-subrc = 0.
    READ TABLE t_0001 INTO DATA(w_0001) INDEX 1.

    IF sy-subrc = 0.
      w_carga-placa_cav   = w_0001-placa_cav.

      SELECT SINGLE name1
        INTO @DATA(l_name1)
        FROM lfa1
       WHERE lifnr = @w_0001-motorista.
      IF sy-subrc = 0.
        w_carga-motorista = l_name1.
      ENDIF.
    ENDIF.

    LOOP AT t_0001 INTO w_0001.
      SELECT SINGLE auart
        INTO @DATA(l_auart)
        FROM vbak
       WHERE vbeln = @w_0001-vbeln.

      IF sy-subrc = 0 AND l_auart <> 'ZRFL'.
        w_carga-kunnr     = w_0001-id_cli_dest.
*     ELSE.
*       CLEAR w_carga-kunnr.
      ENDIF.
    ENDLOOP.

    MODIFY t_carga FROM w_carga INDEX 1.
  ENDIF.

ENDFORM.

************************************************************************
*&-selecao dos fardos
************************************************************************
*FORM f_selecao_dados CHANGING p_erro.
*
*  CLEAR p_erro.
*
*  READ TABLE t_carga INTO w_carga INDEX 1.
*
**------------------------------------------
**- selecao dos fardos a processar
**------------------------------------------
*  SELECT nr_romaneio vbeln branch doc_rem id_cli_dest
*    FROM zsdt0001
*    INTO TABLE tg_zsdt0001
*   WHERE tp_movimento  EQ c_s
*     AND ch_referencia EQ w_carga-ch_referencia
*     AND branch        EQ w_carga-werks
*     AND nr_safra      EQ w_carga-safra.
*
*  CHECK  sy-subrc = 0.
*
*  SELECT vbrp~vbeln vgbel matnr
*    FROM vbrp
*    INNER JOIN vbrk ON vbrk~vbeln = vbrp~vbeln
*    INTO TABLE tg_vbrp
*     FOR ALL ENTRIES IN tg_zsdt0001
*   WHERE vgbel EQ tg_zsdt0001-doc_rem
*     AND fksto EQ abap_off.
*
*  IF sy-subrc IS INITIAL.
*    LOOP AT tg_vbrp  INTO wg_vbrp.
*      wg_vbrp-refkey    = wg_vbrp-vbeln.
*      MODIFY tg_vbrp FROM wg_vbrp.
*    ENDLOOP.
*
*    SELECT docnum refkey
*      FROM j_1bnflin
*      INTO TABLE tg_lin
*       FOR ALL ENTRIES IN tg_vbrp
*     WHERE refkey EQ tg_vbrp-refkey.
*
*    IF sy-subrc IS INITIAL.
*      SELECT docnum nfnum9 bukrs branch
*        FROM j_1bnfe_active
*        INTO TABLE tg_bnfe
*         FOR ALL ENTRIES IN tg_lin
*       WHERE docnum EQ tg_lin-docnum
*         AND nfnum9 <> ' '.
*
*      IF tg_bnfe[] IS INITIAL.
*        p_erro     = abap_true.
*        l_mensagem = 'Para Gerar o lote de Venda a NF-e do Romaneio informado deve estar Determinada'.
*        PERFORM f_gera_erro_carga USING '2' l_mensagem.
*        EXIT.
*      ENDIF.
*    ENDIF.
*  ELSE.
*    p_erro     = abap_true.
*    l_mensagem   = 'Remessa/Fatura ainda não foram gerados!'.
*    PERFORM f_gera_erro_carga USING '2' l_mensagem.
*    EXIT.
*  ENDIF.
*
*  IF p_email = abap_off AND p_integ = abap_off.
*    SELECT werks lgort nr_romaneio charg menge werks_orig
*           lgortr safra mblnr mjahr dados_contingencia  "*-S4H-US 123905-25.09.2023-JT
*      FROM zmmt0008
*      INTO TABLE tg_zmmt0008
*       FOR ALL ENTRIES IN t_lote
*     WHERE   werks       EQ w_carga-werks
*       AND   lgort       EQ w_carga-lgort
*       AND   charg       EQ t_lote-acharg
*       AND   safra       EQ w_carga-safra
*       AND ( nr_romaneio EQ space OR
*             nr_romaneio EQ c_0   OR
*             nr_romaneio EQ w_carga-nr_romaneio ).
*  ELSE.
*    SELECT werks lgort nr_romaneio charg menge werks_orig
*           lgortr safra mblnr mjahr dados_contingencia  "*-S4H-US 123905-25.09.2023-JT
*      FROM zmmt0008
*      INTO TABLE tg_zmmt0008
*       FOR ALL ENTRIES IN t_lote
*     WHERE   werks       EQ w_carga-werks
*       AND   lgort       EQ w_carga-lgort
*       AND   charg       EQ t_lote-acharg
*       AND   safra       EQ w_carga-safra
*       AND   nr_romaneio EQ w_carga-nr_romaneio.
*  ENDIF.
*
*  IF tg_zmmt0008[] IS INITIAL.
*    p_erro       = abap_true.
*    l_mensagem   = 'Formação de Bloco(ZMM0023) não realizado para a carga!(Tabela ZMMT0008 vazia)!'.
*    PERFORM f_gera_erro_carga USING '2' l_mensagem.
*    EXIT.
*  ENDIF.
*
*  IF tg_zmmt0008[] IS INITIAL.
*    DATA: lva_count TYPE i.
*
*    SELECT werks lgort nr_romaneio charg menge werks_orig
*           lgortr safra mblnr mjahr dados_contingencia  "*-S4H-US 123905-25.09.2023-JT
*      FROM zmmt0008
*      INTO TABLE tg_zmmt0008_aux
*     WHERE werks       EQ w_carga-werks
*       AND lgort       EQ w_carga-lgort
*       AND safra       EQ w_carga-safra
*       AND nr_romaneio EQ w_carga-nr_romaneio
*       AND placa_cav   EQ w_carga-placa_cav
*       AND kunnr       EQ w_carga-kunnr
*       AND status      EQ '3'.
*
*    DESCRIBE TABLE tg_zmmt0008_aux LINES lva_count.
*
*    IF lva_count > 0.
*      p_erro     = abap_true.
*      l_mensagem = 'Romaneio está em processo de cancelamento' &&
*                   ' e estorno no sistema Rondonline,'         &&
*                   ' por favor aguardar!'.
*      PERFORM f_gera_erro_carga USING '2' l_mensagem.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  SELECT kunnr name1
*    FROM kna1
*    INTO TABLE tg_kna1
*     FOR ALL ENTRIES IN tg_zsdt0001
*   WHERE kunnr EQ tg_zsdt0001-id_cli_dest.
*
*ENDFORM.

************************************************************************
* organizar dados
************************************************************************
*FORM f_organiza_dados.
*
*  FREE: tg_saida, wg_saida.
*
*  READ TABLE t_carga INTO w_carga INDEX 1.
*
*  LOOP AT tg_vbrp INTO wg_vbrp.
*
*    CLEAR: wg_lin, wg_bnfe, wg_zsdt0001.
*
*    READ TABLE tg_lin INTO wg_lin WITH KEY refkey = wg_vbrp-refkey.
*
*    IF sy-subrc IS INITIAL.
*      READ TABLE tg_bnfe     INTO wg_bnfe     WITH KEY docnum  = wg_lin-docnum.
*      CHECK sy-subrc = 0.
*
*      READ TABLE tg_zsdt0001 INTO wg_zsdt0001 WITH KEY doc_rem = wg_vbrp-vgbel.
*
*      LOOP AT tg_zmmt0008          INTO wg_zmmt0008.
*        READ TABLE t_zsdt0330      INTO w_zsdt0330 WITH KEY werks  = wg_zmmt0008-werks
*                                                            lgort  = wg_zmmt0008-lgort
*                                                            acharg = wg_zmmt0008-charg
*                                                            safra  = wg_zmmt0008-safra.
*        CHECK sy-subrc = 0.
*
*        MOVE: abap_true              TO wg_saida-mark,
*              wg_zmmt0008-werks      TO wg_saida-werks,
*              wg_zmmt0008-lgort      TO wg_saida-bloco,
*              wg_zmmt0008-charg      TO wg_saida-fardo,
*              w_carga-nr_romaneio    TO wg_saida-nr_romaneio,
*              wg_bnfe-nfnum9         TO wg_saida-nfnum,
*              wg_zsdt0001-vbeln      TO wg_saida-vbeln,
*              wg_vbrp-vbeln          TO wg_saida-vbeln_vf,
*              wg_vbrp-matnr          TO wg_saida-matnr,
*              w_carga-placa_cav      TO wg_saida-placa_cav,
*              w_carga-ch_referencia  TO wg_saida-ch_ref,
*              wg_zmmt0008-menge      TO wg_saida-menge,
*              wg_zmmt0008-werks_orig TO wg_saida-werks_orig,
*              wg_zmmt0008-lgortr     TO wg_saida-lgortr, "*-S4H-US 123905-25.09.2023-JT
*              wg_zmmt0008-safra      TO wg_saida-safra,  "*-S4H-US 123905-25.09.2023-JT
*              wg_zmmt0008-mblnr      TO wg_saida-mblnr,  "*-S4H-US 123905-25.09.2023-JT
*              wg_zmmt0008-mjahr      TO wg_saida-mjahr,  "*-S4H-US 123905-25.09.2023-JT
*              wg_zmmt0008-dados_contingencia TO wg_saida-dados_contingencia. "*-S4H-US 123905-25.09.2023-JT
*
*        APPEND wg_saida              TO tg_saida.
*        CLEAR: wg_saida.
*      ENDLOOP.
*
*      CLEAR: wg_vbrp, wg_lin, wg_zsdt0001, wg_bnfe.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.

************************************************************************
* atualiza zmmt0008
************************************************************************
*FORM f_atualiza_zmmt0008.
*
*  DATA: tl_0008        TYPE TABLE OF zmmt0008 WITH HEADER LINE,
*        w_werks        TYPE zmmt0008-werks,
*        l_interc       TYPE c,
*        wl_cont        TYPE sy-tabix,
*        msg(150),
*        resposta,
*        wl_cont_aux(6).
*
*  TYPES: tt_selection TYPE STANDARD TABLE OF rsparams.
*
*  DATA: l_selection  TYPE rsparams,
*        li_selection TYPE tt_selection.
*
*  READ TABLE t_carga  INTO w_carga  INDEX 1.
*
*  READ TABLE tg_saida INTO wg_saida WITH KEY mark  = c_x
*                                             nfnum = 0.
*  IF sy-subrc IS INITIAL.
*    l_mensagem = 'Não localizado número da Nota para o romaneio informado,' &&
*                 'verifique se a mesma esta determinada!'.
*    PERFORM f_gera_erro_carga USING '2' l_mensagem.
*    EXIT.
*  ENDIF.
*
*  SELECT SINGLE kunnr, ktokd
*    FROM kna1
*    INTO @DATA(w_kna1)
*   WHERE kunnr = @w_carga-kunnr.
*
*  IF sy-subrc = 0 AND w_kna1-ktokd = 'ZCIC'.
*    l_interc = abap_true.
*
*    IF tg_saida[] IS NOT INITIAL.
*      SELECT *
*        FROM zmmt0008
*        INTO TABLE @DATA(t_0008)
*         FOR ALL ENTRIES IN @tg_saida
*       WHERE werks = @w_kna1-kunnr+6(4)
*         AND lgort = @tg_saida-bloco
*         AND charg = @tg_saida-fardo
*         AND safra = @tg_saida-safra.
*    ENDIF.
*  ENDIF.
*
**------------------------------------------
** atualizar zmmt0008
**------------------------------------------
*  LOOP AT tg_saida INTO wg_saida  WHERE mark EQ c_x.
*
*    CLEAR tl_0008.
*
*    SELECT SINGLE *
*      FROM zmmt0008 INTO @DATA(lwa_zmmt0008_exists)
*     WHERE werks EQ @wg_saida-werks
*       AND lgort EQ @wg_saida-bloco
*       AND charg EQ @wg_saida-fardo
*       AND safra EQ @wg_saida-safra.
*
*    IF sy-subrc EQ 0.
*      tl_0008-cd_sai      = lwa_zmmt0008_exists-cd_sai.
*      tl_0008-tipo_fardo  = lwa_zmmt0008_exists-tipo_fardo.
*      tl_0008-dt_registro = lwa_zmmt0008_exists-dt_registro.
*      tl_0008-hr_registro = lwa_zmmt0008_exists-hr_registro.
*    ENDIF.
*
*    MOVE: wg_saida-werks       TO w_werks.
*    MOVE: sy-mandt             TO tl_0008-mandt,
*          wg_saida-werks       TO tl_0008-werks,
*          wg_saida-bloco       TO tl_0008-lgort,
*          wg_saida-fardo       TO tl_0008-charg,
*          wg_saida-nr_romaneio TO tl_0008-nr_romaneio,
*          wg_saida-nfnum       TO tl_0008-nfnum,
*          wg_saida-vbeln       TO tl_0008-vbeln,
*          wg_saida-vbeln_vf    TO tl_0008-vbeln_vf,
*          wg_saida-placa_cav   TO tl_0008-placa_cav,
*          wg_saida-menge       TO tl_0008-menge,
*          wg_saida-werks_orig  TO tl_0008-werks_orig,
*          wg_saida-matnr       TO tl_0008-matnr, "*-S4H-US 123905-25.09.2023-JT
*          wg_saida-lgortr      TO tl_0008-lgortr, "*-S4H-US 123905-25.09.2023-JT
*          wg_saida-safra       TO tl_0008-safra,  "*-S4H-US 123905-25.09.2023-JT
*          wg_saida-mblnr       TO tl_0008-mblnr,  "*-S4H-US 123905-25.09.2023-JT
*          wg_saida-mjahr       TO tl_0008-mjahr,  "*-S4H-US 123905-25.09.2023-JT
*          wg_saida-dados_contingencia TO tl_0008-dados_contingencia, "*-S4H-US 123905-25.09.2023-JT
*          w_carga-motorista    TO tl_0008-motorista,
*          w_carga-kunnr        TO tl_0008-kunnr,
*          sy-datum             TO tl_0008-dt_inicial_integ,
*          '1'                  TO tl_0008-status.
*
*    APPEND tl_0008.
*
*    CLEAR l_selection.
*    l_selection-selname  = 'S_WERKS'.          "Option name
*    l_selection-kind     = 'S'.                "S= select options P=Parameters
*    l_selection-sign     = 'I'.                "Sign
*    l_selection-option   = 'EQ'.               "Option
*    l_selection-low      = w_werks .           "*-CS2022000332-#78518 -30.04.2022-JT-inicio
*
*    APPEND l_selection TO li_selection.
*
*    CLEAR l_selection.
*    l_selection-selname  = 'S_LGORT'.
*    l_selection-kind     = 'S'.
*    l_selection-sign     = 'I'.
*    l_selection-option   = 'EQ'.
*    l_selection-low      = tl_0008-lgort .
*
*    APPEND l_selection TO li_selection.
*
*    CLEAR l_selection.
*    l_selection-selname  = 'S_VBN_VF'.
*    l_selection-kind     = 'S'.
*    l_selection-sign     = 'I'.
*    l_selection-option   = 'EQ'.
*    l_selection-low      = tl_0008-vbeln_vf .
*
*    APPEND l_selection TO li_selection.
*
*    CLEAR l_selection.
*    l_selection-selname  = 'S_PLCAV'.
*    l_selection-kind     = 'S'.
*    l_selection-sign     = 'I'.
*    l_selection-option   = 'EQ'.
*    l_selection-low      = tl_0008-placa_cav .
*
*    APPEND l_selection TO li_selection.
*
*    IF l_interc = abap_true AND w_kna1-kunnr+6(4) NE w_werks.
*      READ TABLE t_0008 INTO DATA(w_0008) WITH KEY werks = w_kna1-kunnr+6(4)
*                                                   lgort = wg_saida-bloco
*                                                   charg = wg_saida-fardo
*                                                   safra = wg_saida-safra.
*      IF ( sy-subrc <> 0 ) OR ( sy-subrc = 0 AND w_0008-nr_romaneio IS INITIAL ).
*        CLEAR tl_0008.
*
*        tl_0008-cd_sai      = lwa_zmmt0008_exists-cd_sai.
*        tl_0008-tipo_fardo  = lwa_zmmt0008_exists-tipo_fardo.
*        tl_0008-dt_registro = lwa_zmmt0008_exists-dt_registro.
*        tl_0008-hr_registro = lwa_zmmt0008_exists-hr_registro.
*
*        MOVE:  sy-mandt          TO tl_0008-mandt,
*               w_kna1-kunnr+6(4) TO tl_0008-werks,
*               wg_saida-werks    TO tl_0008-werks_orig, "*-CS2022000332-#78803-05.07.2022-JT-inicio
*               wg_saida-bloco    TO tl_0008-lgort,
*               wg_saida-fardo    TO tl_0008-charg,
*               wg_saida-menge    TO tl_0008-menge,
*               wg_saida-matnr    TO tl_0008-matnr, "*-S4H-US 123905-25.09.2023-JT
*               wg_saida-lgortr   TO tl_0008-lgortr, "*-S4H-US 123905-25.09.2023-JT
*               wg_saida-safra    TO tl_0008-safra,  "*-S4H-US 123905-25.09.2023-JT
**              wg_saida-mblnr    TO tl_0008-mblnr,  "*-S4H-US 123905-25.09.2023-JT
**              wg_saida-mjahr    TO tl_0008-mjahr,  "*-S4H-US 123905-25.09.2023-JT
*               wg_saida-dados_contingencia TO tl_0008-dados_contingencia. "*-S4H-US 123905-25.09.2023-JT
*        APPEND tl_0008.
*
*        CLEAR l_selection.
*        l_selection-selname = 'S_WERKS'.          "Option name
*        l_selection-kind    = 'S'.                "S= select options P=Parameters
*        l_selection-sign    = 'I'.                "Sign
*        l_selection-option  = 'EQ'.               "Option
*        l_selection-low     = tl_0008-werks . "   Value
*        APPEND l_selection TO li_selection.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.
*
*  IF p_unico = abap_false OR p_repro = abap_true.
*    MODIFY zmmt0008 FROM TABLE tl_0008.
*
**-----------------------------
**---atualiza status
**-----------------------------
*    l_mensagem = 'Geração de Lote efetuada com Sucesso'.
*    PERFORM f_monta_log     USING '3'
*                                  abap_off
*                                  l_mensagem.
*  ELSE.
*    PERFORM  f_atual_status USING '3'.
*  ENDIF.
*
*  COMMIT WORK AND WAIT.
*
**------------------------------------------
** envio API
**------------------------------------------
*  SORT li_selection BY selname kind sign option low  .
*  DELETE ADJACENT DUPLICATES FROM li_selection
*                        COMPARING ALL FIELDS.
*
*  IF ( p_unico = abap_false AND p_repro = abap_false AND p_email = abap_false AND p_integ = abap_false ) OR
*     ( p_unico = abap_true  AND ( p_integ = abap_true OR p_repro = abap_true ) ).
*    SUBMIT zmmr162 WITH SELECTION-TABLE li_selection
*                   WITH s_acao EQ 'I'
*                    AND RETURN.
*  ENDIF.
*
*  IF ( p_unico = abap_false AND p_repro = abap_false AND p_email = abap_false AND p_integ = abap_false ) OR
*     ( p_unico = abap_true  AND ( p_email = abap_true OR p_repro = abap_true ) ).
*    PERFORM  f_seleciona_dados_email.
*  ENDIF.
*
*ENDFORM.

************************************************************************
* monta log
************************************************************************
FORM f_monta_log USING p_status
                       p_status_email
                       p_mensagem.

  LOOP AT tg_saida INTO wg_saida  WHERE mark EQ c_x.
    READ TABLE t_zsdt0330 INTO w_zsdt0330 WITH KEY ch_referencia = wg_saida-ch_ref
                                                   werks         = wg_saida-werks
                                                   lgort         = wg_saida-bloco
                                                   acharg        = wg_saida-fardo.
    CHECK sy-subrc = 0.

    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_log( EXPORTING i_id_carga     = w_zsdt0330-id_carga
                                    i_matnr        = w_zsdt0330-matnr
                                    i_werks        = w_zsdt0330-werks
                                    i_lgort        = w_zsdt0330-lgort
                                    i_acharg       = w_zsdt0330-acharg
                                    i_safra        = w_zsdt0330-safra
                                    i_tipo_integra = 'GL'
                                    i_tipo_msg     = COND #( WHEN p_status = 3 THEN 'S'
                                                                               ELSE 'E' )
                                    i_mensagem     = p_mensagem ).

    UPDATE zsdt0330 SET status_gera_lote = p_status
                        status_email_gl  = p_status_email
                  WHERE id_carga         = w_zsdt0330-id_carga
                    AND matnr            = w_zsdt0330-matnr
                    AND werks            = w_zsdt0330-werks
                    AND lgort            = w_zsdt0330-lgort
                    AND acharg           = w_zsdt0330-acharg
                    AND safra            = w_zsdt0330-safra
                    AND cancelado        = abap_off.
  ENDLOOP.

ENDFORM.

************************************************************************
* monta log
************************************************************************
FORM f_atual_status    USING p_status.

  LOOP AT tg_saida INTO wg_saida  WHERE mark EQ c_x.
    READ TABLE t_zsdt0330 INTO w_zsdt0330 WITH KEY ch_referencia = wg_saida-ch_ref
                                                   werks         = wg_saida-werks
                                                   lgort         = wg_saida-bloco
                                                   acharg        = wg_saida-fardo.
    CHECK sy-subrc = 0.

    UPDATE zsdt0330 SET status_gera_lote = p_status
                  WHERE id_carga         = w_zsdt0330-id_carga
                    AND matnr            = w_zsdt0330-matnr
                    AND werks            = w_zsdt0330-werks
                    AND lgort            = w_zsdt0330-lgort
                    AND acharg           = w_zsdt0330-acharg
                    AND safra            = w_zsdt0330-safra
                    AND cancelado        = abap_off.
  ENDLOOP.

ENDFORM.

************************************************************************
* envio email
************************************************************************
*FORM f_seleciona_dados_email.
*
*  FREE: tg_excel, wg_excel, wg_fardos_exc, tg_fardos_exc.
*
*  READ TABLE t_carga  INTO w_carga  INDEX 1.
*
*  SELECT *
*    FROM zmmt0008
*    INTO TABLE @DATA(it_0008)
*   WHERE werks       EQ @w_carga-werks
*     AND lgort       EQ @w_carga-lgort
*     AND nr_romaneio EQ @w_carga-nr_romaneio
*     AND safra       EQ @w_carga-safra.
*
*  IF sy-subrc IS INITIAL.
*    READ TABLE it_0008 INTO DATA(wa_0008) INDEX 1.
*    wg_excel-veiculo      = wa_0008-placa_cav.
*    wg_excel-romaneio     = wa_0008-nr_romaneio.
*    wg_excel-motorista    = wa_0008-motorista.
*    wg_excel-bloco        = wa_0008-lgort.
*
*    SELECT SINGLE name1
*      FROM kna1
*      INTO wg_excel-cliente
*     WHERE kunnr EQ wa_0008-kunnr.
*
*    SELECT SINGLE *
*      FROM zsdt0001
*     WHERE vbeln        EQ wa_0008-vbeln
*       AND tp_movimento EQ c_s
*       AND nr_romaneio  EQ w_carga-nr_romaneio
*       AND branch       EQ w_carga-werks
*       AND vbeln        EQ wa_0008-vbeln.
*
*    IF sy-subrc IS INITIAL.
*      wg_excel-safra = zsdt0001-nr_safra.
*
*      SELECT SINGLE *
*        FROM zsdt0066
*        INTO @DATA(wa_zsdt0066)
*       WHERE vbeln EQ @zsdt0001-vbeln.
*
**---- Envio de Romaneio de Saída (Algodoeira)
*      IF sy-subrc IS INITIAL. "busca dados baseado na formação de lote
*        CLEAR wa_zsdt0045.
*
*        SELECT SINGLE zseq_inst objek objecttable booking instrucao
*          FROM zsdt0045
*          INTO wa_zsdt0045
*         WHERE objek      EQ wa_zsdt0066-nro_sol_ov
*           AND instrucao  EQ wa_zsdt0066-instrucao.
*
*        SELECT *
*          FROM zmmt0126
*          INTO TABLE @DATA(t_zmmt0126)
*         WHERE kunnr EQ @wa_zsdt0066-lentrega.
*
*        wg_excel-instrucao = wa_zsdt0066-instrucao.
*
*      ELSE. "busca dados baseado na venda ZFEX
*        SELECT SINGLE *
*          FROM zsdt0053
*          INTO @DATA(wa_zsdt0053)
*         WHERE vbeln EQ @zsdt0001-vbeln.
*
*        CLEAR wa_zsdt0045.
*
*        SELECT SINGLE zseq_inst objek objecttable booking instrucao tamanho_fardo
*          FROM zsdt0045
*          INTO wa_zsdt0045
*         WHERE objek      EQ wa_zsdt0053-nro_sol_ov
*           AND instrucao  EQ wa_zsdt0053-instrucao.
*
*        wg_excel-instrucao = wa_zsdt0053-instrucao.
*
*      ENDIF.
*
***    Destinatario
*      SELECT SINGLE name1
*        FROM kna1
*        INTO wg_excel-terminal
*       WHERE kunnr EQ zsdt0001-id_cli_dest.
*
*      SELECT SINGLE *
*        FROM j_1bnflin
*       WHERE refkey EQ wa_0008-vbeln_vf.
*
*      IF sy-subrc IS INITIAL.
*        SELECT SINGLE *
*          FROM j_1bnfe_active
*         WHERE docnum EQ j_1bnflin-docnum.
*
*        IF sy-subrc IS INITIAL.
***          Nota Fiscal
*          wg_excel-nfnum = j_1bnfe_active-nfnum9.
*
*          SELECT SINGLE *
*            FROM j_1bbranch
*           WHERE bukrs  EQ j_1bnfe_active-bukrs
*             AND branch EQ j_1bnfe_active-branch.
*** Remetente
*          wg_excel-remetente = j_1bbranch-name.
*          wg_excel-stcd1     = j_1bbranch-stcd1.
*
*          SELECT SINGLE butxt
*            FROM t001
*            INTO wg_excel-empresa
*           WHERE bukrs EQ j_1bnfe_active-bukrs.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    SELECT vbeln vgbel matnr
*      FROM vbrp
*      INTO TABLE tg_vbrp
*     WHERE vbeln EQ wa_0008-vbeln_vf.
*
*    IF sy-subrc IS INITIAL.
*      SELECT matnr normt
*        FROM mara
*        INTO TABLE tg_mara
*         FOR ALL ENTRIES IN tg_vbrp
*       WHERE matnr EQ tg_vbrp-matnr.
*    ENDIF.
*
*    LOOP AT it_0008 INTO wa_0008.
*      LOOP AT tg_mara INTO wg_mara.
*
*        "Projeto Reestruturação Algodao 2024
**        SELECT SINGLE *
**          FROM zppt0002
**          INTO @DATA(wa_0002)
**         WHERE acharg EQ @wa_0008-charg
**           AND lgort   = @wa_0008-lgort
**           AND werks   = @wa_0008-werks.
**
**        IF wa_0002 IS NOT INITIAL.
**          SELECT SINGLE *
**            FROM zppt0004
**            INTO @DATA(wa_0004)
**           WHERE werks EQ @wa_0002-werks
**             AND verid EQ @wa_0002-verid.
**        ENDIF.
*        "Projeto Reestruturação Algodao 2024
*
*        CONDENSE wa_0008-charg NO-GAPS.
*        wg_fardos_exc-fardos  = |'{ wa_0008-cd_sai }|.
*        wg_fardos_exc-tipo    = wg_mara-normt(5).
*        wg_fardos_exc-peso    = wa_0008-menge.
*        wg_fardos_exc-tamanho = wa_0008-tipo_fardo.
*
*        IF wg_fardos_exc-tamanho IS INITIAL.
*          wg_fardos_exc-tamanho = wa_zsdt0045-tamanho_fardo.
*        ENDIF.
*
*        APPEND wg_fardos_exc TO tg_fardos_exc.
*      ENDLOOP.
*      CLEAR: wa_0008.
*    ENDLOOP.
*
*    LOOP AT it_0008 INTO wa_0008.
*      CONDENSE wa_0008-charg NO-GAPS.
*      wg_excel-qtd_fardos  = wg_excel-qtd_fardos + 1.
*      wg_excel-peso_total  = wg_excel-peso_total  + wa_0008-menge.
*      CLEAR: wa_0008.
*    ENDLOOP.
*    APPEND wg_excel  TO tg_excel.
*  ENDIF.
*
*  SELECT *
*    FROM zmail
*    INTO TABLE it_zmail
*   WHERE werks EQ w_carga-werks
*     AND tcode EQ 'ZMM0027'.
*
*  LOOP AT t_zmmt0126 INTO DATA(w_zmmt0126).
*
*    IF w_zmmt0126-email IS NOT INITIAL.
*      wa_zmail-email   = w_zmmt0126-email.
*      wa_zmail-usuario = w_zmmt0126-usnam.
*      APPEND wa_zmail TO it_zmail.
*      CLEAR wa_zmail.
*    ENDIF.
*
*  ENDLOOP.
*
*  DELETE ADJACENT DUPLICATES FROM it_zmail COMPARING email.
*
*  PERFORM f_monta_excel.
*
*ENDFORM.

************************************************************************
* montar excel
************************************************************************
*FORM f_monta_excel.
*
*  DATA: lv_string   TYPE string,
*        vqtd_fardos TYPE string,
*        vpeso_total TYPE string,
*        vpeso       TYPE string,
*        vcnpj(18)   TYPE c.
*
*  CONSTANTS: gc_tab  TYPE c VALUE cl_bcs_convert=>gc_tab,
*             gc_tab2 TYPE c VALUE cl_abap_char_utilities=>vertical_tab,
*             gc_crlf TYPE c VALUE cl_bcs_convert=>gc_crlf.
*
*  CHECK tg_excel[] IS NOT INITIAL.
*
*  LOOP AT tg_excel INTO wg_excel.
*
*    vqtd_fardos = wg_excel-qtd_fardos.
*    vpeso_total = wg_excel-peso_total.
*
*    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
*      EXPORTING
*        input  = wg_excel-stcd1
*      IMPORTING
*        output = vcnpj.
*
*    CONCATENATE
*    'Empresa:'          gc_tab    wg_excel-empresa        gc_crlf
*    'Safra:'            gc_tab    wg_excel-safra          gc_crlf
*    'Remetente:'        gc_tab    wg_excel-remetente      gc_crlf
*    'CNPJ Remetente'    gc_tab    vcnpj                   gc_crlf
*    'Terminal:'         gc_tab    wg_excel-terminal       gc_crlf
*    'Cliente:'          gc_tab    wg_excel-cliente        gc_crlf
*    'Romaneio:'         gc_tab    wg_excel-romaneio       gc_crlf
*    'NFe.F.Lote:'       gc_tab    wg_excel-nfnum          gc_crlf
*    'Motorista:'        gc_tab    wg_excel-motorista      gc_crlf
*    'Veículo (Placa):'  gc_tab    wg_excel-veiculo        gc_crlf
*    'Lote (Bloco):'     gc_tab    wg_excel-bloco          gc_crlf
*    'Qtd Fardos:'       gc_tab    vqtd_fardos             gc_crlf
*    'Peso Total:'       gc_tab    vpeso_total             gc_crlf
*    'Instrução:'        gc_tab    wg_excel-instrucao      gc_crlf
*    INTO lv_string.
*
*    CLEAR:  vqtd_fardos, vpeso_total.
*  ENDLOOP.
*
*  CONCATENATE lv_string       gc_crlf
*                              gc_crlf
*              'Nr. Fardo'     gc_tab
*              'Tipo'          gc_tab
*              'Peso'          gc_tab
*              'Tamanho'       gc_crlf
*         INTO lv_string.
*
*  LOOP AT tg_fardos_exc INTO wg_fardos_exc.
*
*    vpeso =   wg_fardos_exc-peso.
*    CONCATENATE  lv_string
*                 wg_fardos_exc-fardos   gc_tab
*                 wg_fardos_exc-tipo     gc_tab
*                 vpeso                  gc_tab
*                 wg_fardos_exc-tamanho  gc_crlf
*           INTO lv_string.
*
*    CLEAR: wg_fardos_exc, vpeso.
*  ENDLOOP.
*
*  CLEAR: size, binary_content.
*
*  TRY.
*      cl_bcs_convert=>string_to_solix(
*        EXPORTING
*          iv_string   = lv_string
*          iv_codepage = '4103'
*          iv_add_bom  = 'X'
*        IMPORTING
*          et_solix    = binary_content
*          ev_size     = size ).
*    CATCH cx_bcs.
*      MESSAGE e445(so).
*  ENDTRY.
*
*  IF it_zmail[] IS NOT INITIAL.
*    PERFORM f_send.
*  ELSE.
*    l_mensagem = 'Romaneio gerado, mas não foram encontrados e-mail cadastrados.'.
*    PERFORM f_monta_log USING '3'
*                              abap_off
*                              l_mensagem.
*    COMMIT WORK.
*  ENDIF.
*
*ENDFORM.
*
*************************************************************************
** send email
*************************************************************************
*FORM f_send.
*
*  DATA: mailto  TYPE ad_smtpadr,
*        subject TYPE so_obj_des.
*
*  DATA: vuser TYPE sy-uname.
*
*  DATA: l_romaneio TYPE c LENGTH 9.
*
*  CLEAR:  main_text, subject, vuser, l_romaneio.
*
*  l_romaneio =  wg_excel-romaneio.
*  PACK l_romaneio TO l_romaneio. CONDENSE l_romaneio.
*  CONCATENATE 'Romaneio' l_romaneio 'SI' wg_excel-instrucao INTO subject SEPARATED BY space.
*
*  TRY.
*      send_request = cl_bcs=>create_persistent( ).
*
*      PERFORM f_tabela_info_adic.
*
*      document = cl_document_bcs=>create_document(
*        i_type    = 'HTM' "RAW
*        i_text    = main_text
*        i_subject = subject ).
*
*      document->add_attachment(
*        i_attachment_type    = 'xls'
*        i_attachment_subject = subject
*        i_attachment_size    = size
**       I_ATT_CONTENT_TEXT   = gt_con
*        i_att_content_hex    = binary_content ).
*
*      send_request->set_document( document ).
*
*      LOOP AT it_zmail INTO wa_zmail.
*        IF wa_zmail IS NOT INITIAL.
*          CLEAR mailto.
*
*          mailto = wa_zmail-email.
*          recipient = cl_cam_address_bcs=>create_internet_address( mailto ).
*          send_request->add_recipient( recipient ).
*        ENDIF.
*      ENDLOOP.
*
*      vuser = sy-uname.
*      sy-uname = 'JOBADM'.
*      sent_to_all = send_request->send( i_with_error_screen = 'X' ).
*
*      COMMIT WORK.
*      sy-uname = vuser.
*
**     IF sent_to_all IS INITIAL.
**       MESSAGE s026(so).
**     ELSE.
**       MESSAGE s022(so).
**     ENDIF.
*
*    CATCH cx_bcs INTO bcs_exception.
*      l_mensagem = 'Erro ao enviar email'.
*      PERFORM f_monta_log USING '3'
*                                abap_off
*                                l_mensagem.
*      EXIT.
*  ENDTRY.
*
*  l_mensagem = 'Email enviado com Sucesso'.
*  PERFORM f_monta_log USING '3'
*                            abap_on
*                            l_mensagem.
*
*ENDFORM.
*
*************************************************************************
** info adicional
*************************************************************************
*FORM f_tabela_info_adic .
*
*  DATA: gs_main_text LIKE LINE OF main_text.
*
*  MOVE '<BR>' TO gs_main_text-line.
*  APPEND gs_main_text TO main_text.
*  CLEAR gs_main_text.
*
*  CONCATENATE 'Segue em anexo o Romaneio de Saída de Fardos.'(014) '<BR><BR>'
*INTO gs_main_text-line SEPARATED BY space.
*  APPEND gs_main_text TO main_text.  CLEAR gs_main_text.
*
*  MOVE '<TABLE BORDER=1>' TO gs_main_text-line.
*  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
*
**col1
*  MOVE '<TR style="background-color: #96A5AA;"> <TH> Romaneio </TH> ' TO gs_main_text-line.
*  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
*
**col2
*  MOVE '<TH> Instrução </TH> ' TO gs_main_text-line.
*  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
*
**col3
*  MOVE '<TH> Booking </TH></TR>' TO gs_main_text-line.
*  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
*
**col1
*  IF wg_excel-romaneio IS NOT INITIAL.
*    CONCATENATE '<TR> <TD>' wg_excel-romaneio '</TD>' INTO gs_main_text-line.
*    APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
*  ELSE.
*    MOVE '<TR> <TD></TD>' TO gs_main_text-line.
*    APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
*  ENDIF.
*
**col2
*  CONCATENATE '<TD>' wg_excel-instrucao '</TD>' INTO gs_main_text-line.
*  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
*
**col3
*  CONCATENATE '<TD>' wa_zsdt0045-booking '</TD></TR>' INTO gs_main_text-line.
*  APPEND gs_main_text TO main_text. CLEAR gs_main_text.
*
*
*  MOVE '</TABLE>' TO gs_main_text-line.
*  APPEND gs_main_text TO main_text.  CLEAR gs_main_text.
*
*  MOVE '<BR>' TO gs_main_text-line.
*  APPEND gs_main_text TO main_text.
*  CLEAR gs_main_text.
*
*  CONCATENATE '<BR>' 'Atenciosamente'(013) '<BR>'
*  INTO gs_main_text-line.
*  APPEND gs_main_text TO main_text. CLEAR gs_main_text.
*
*  APPEND '  '     TO main_text.
*
*ENDFORM.

************************************************************************
************************************************************************
