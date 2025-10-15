*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 08.07.2022                                              &*
*& Descrição: Processamento Transferencia FArdos - Trace Cotton       &*
*&--------------------------------------------------------------------&*
*& Projeto  : Algodão                                                 &*
*&--------------------------------------------------------------------&*
REPORT zsdr0152_job MESSAGE-ID zjob.

************************************************************************
* tabelas
************************************************************************
TABLES: zsdt0330.

************************************************************************
*  parametro ID_CARGA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
  PARAMETERS : p_unico  AS CHECKBOX,
               p_ch_ref LIKE zsdt0330-chave_referencia,
               p_idcar  LIKE zsdt0330-id_carga,
               "p_seq    LIKE zsdt0330-seq,           "SD - Ganho Peso Automatico Algodao US #145369 - WPP
               "p_ch_rom LIKE zsdt0330-ch_referencia, "SD - Ganho Peso Automatico Algodao US #145369 - WPP
               p_matnr  LIKE zsdt0330-matnr,
               p_werks  LIKE zsdt0330-werks,
               p_lgort  LIKE zsdt0330-lgort,
               p_acharg LIKE zsdt0330-acharg,
               p_safra  LIKE zsdt0330-safra.
SELECTION-SCREEN END   OF BLOCK b1.

************************************************************************
*& types
************************************************************************
TYPES: BEGIN OF ty_carga,
         id_carga TYPE zsdt0330-id_carga,
         matnr    TYPE zsdt0330-matnr,
         werks    TYPE zsdt0330-werks,
         lgort    TYPE zsdt0330-lgort,
         acharg   TYPE zsdt0330-acharg,
         safra    TYPE zsdt0330-safra,
         cd_sai   TYPE zsdt0330-cd_sai,
         seq      TYPE zsdt0330-seq.
TYPES: END OF ty_carga.

TYPES: BEGIN OF type_mchb,
         matnr      TYPE mchb-matnr,
         werks      TYPE mchb-werks,
         lgort      TYPE mchb-lgort,
         charg      TYPE mchb-charg,
         clabs      TYPE mchb-clabs,
         cspem      TYPE mchb-cspem,
         maktx      TYPE makt-maktx,
         lgortr     TYPE mchb-lgort,
         chargr     TYPE mchb-charg,
         matnc      TYPE mchb-matnr,
         cd_sai     TYPE zppt0002-cd_sai,
         icon       TYPE char05,
         log        TYPE char40,
         possuiacts TYPE string,
         status     TYPE string,
       END   OF type_mchb,

       BEGIN OF type_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END   OF type_makt,

       BEGIN OF type_msn,
         tp_msn   TYPE bapi_mtype,
         doc_mat  TYPE bapi2017_gm_head_ret-mat_doc,
         ano      TYPE bapi2017_gm_head_ret-doc_year,
         lote     TYPE charg_d,
         messagem TYPE bapi_msg,
       END   OF type_msn.

************************************************************************
*& variaveis globais
************************************************************************
DATA: t_zsdt0330         TYPE TABLE OF zsdt0330,
      t_0330_carga       TYPE TABLE OF zsdt0330,
      t_carga            TYPE TABLE OF ty_carga,
      t_fardos_mov       TYPE zpps0010_t,
      t_mchb             TYPE TABLE OF type_mchb,
      t_trans            TYPE TABLE OF type_mchb,
      "t_zppt0002         TYPE TABLE OF zppt0002,
      t_zmmt0008         TYPE TABLE OF zmmt0008,
      t_zsdt0330_est     TYPE TABLE OF zsdt0330,
      t_makt             TYPE TABLE OF type_makt,
      t_mseg             TYPE TABLE OF mseg,
      t_msn              TYPE TABLE OF type_msn,
      w_zsdt0330         TYPE zsdt0330,
      w_zsdt0330_est     TYPE zsdt0330,
      w_0330_carga       TYPE zsdt0330,
      w_zppt0002         TYPE zppt0002,
      w_zmmt0008         TYPE zmmt0008,
      w_zmmt0008_delete  TYPE zmmt0008_delete,
      w_carga            TYPE ty_carga,
      w_retorno          TYPE zsdt0331,
      w_mseg             TYPE mseg,
      t_status           TYPE zde_btcstatus_t,
      l_tabix            TYPE sy-tabix,
      l_mensagem         TYPE string,
      l_erro             TYPE char01,
      l_erro_lock        TYPE c,
      is_block           TYPE char01,
      l_index            TYPE i,
      l_fardos           TYPE i,
      v_trace            TYPE char1 VALUE 0,
      v_letrac           TYPE char1 VALUE 0,
      vg_qtde            TYPE p,
      vg_werk            TYPE werks_d,
      vg_cd_sai          TYPE char20,
      vg_lote            TYPE char04,
      vg_lgort           TYPE lgort_d,
      sl_mchb            TYPE type_mchb,
      r_data             TYPE zde_acts_tracecotton_t,
      r_charg            TYPE RANGE OF zppt0002-charg,

*------------------------------
* para estorno movto 261
*------------------------------
      l_plaf             TYPE TABLE OF  plaf WITH HEADER LINE,
      wa_mkal            TYPE mkal,
      mt61d              LIKE  mt61d,
      l_cm61m            LIKE  cm61m,
      eselid             LIKE  af61z-selid,
      xscrap             TYPE  xfeld,
      esbflg             LIKE  cm61x-sbflg,
      mdpmx	             TYPE TABLE OF mdpm WITH HEADER LINE,
      vsaldo             TYPE mchb-clabs,
      sperr_user         TYPE sy-msgv1,
      fg_bloqueio(1),
      vlines             TYPE sy-tabix,
      es_bflushflags     LIKE bapi_rm_flg,
      es_bflushdatagen   LIKE bapi_rm_datgen,
      es_confirmation    LIKE bapi_rm_datkey-confirmation,
      v_confirmation_es  TYPE bapi_rm_datkey-cancconfirmation,
      gs_blpp            LIKE blpp,
      it_goodsmovements  TYPE TABLE OF bapi2017_gm_item_create,
      wa_goodsmovements  TYPE bapi2017_gm_item_create,
      it_bapi_char_batch TYPE TABLE OF bapi_char_batch,
      wa_bapi_char_batch TYPE bapi_char_batch,
      es_goodsmvt_header LIKE bapi2017_gm_head_01,
      es_goodsmvt_code   LIKE bapi2017_gm_code,
      it_return          TYPE TABLE OF bapiret2,
      wa_return          TYPE bapiret2.

"SD - Ganho Peso Automatico Algodao US #145369 - WPP
RANGES: lra_data_carga FOR zsdt0330-data_carga.
DATA: lva_dt_read_carga TYPE zsdt0330-data_carga.
"SD - Ganho Peso Automatico Algodao US #145369 - WPP

************************************************************************
*&      Start-Of-Selection
************************************************************************
START-OF-SELECTION.

  "SD - Ganho Peso Automatico Algodao US #145369 - WPP
  SELECT SINGLE * FROM tvarvc INTO @DATA(lwa_zsdr0153_job) WHERE name = 'ZSDR0153_JOB_DAYS_READ'.
  IF sy-subrc EQ 0.
    lva_dt_read_carga = sy-datum - lwa_zsdr0153_job-low.
    APPEND VALUE #( sign = 'I' option = 'GE' low = lva_dt_read_carga ) TO lra_data_carga.
  ENDIF.
  "SD - Ganho Peso Automatico Algodao US #145369 - WPP


  "##############| ATENÇÃO |##############
  "A sequencia de execução dos forms abaixo, não deve ser alterada

  PERFORM: f_processa_dados,               "Processamento carga normal
           f_ajusta_movimento_sobra_perda, "Processamento Ganho/Perda Peso
           f_selecao_estornos,             "Seleção Registros para estorno
           f_processa_estornos.            "Processar estorno de carregaemnto


************************************************************************
*&-processamento dos estornos
************************************************************************
FORM f_selecao_estornos.

  CHECK p_unico = abap_false.

  CLEAR: t_zsdt0330_est[], t_zsdt0330[].

  SELECT *
    FROM zsdt0330 AS a
    INTO TABLE t_zsdt0330_est
   WHERE chave_referencia = p_ch_ref
     AND status_fardo     = '1'
     AND status_estorno   = 'D'
     AND cancelado        = abap_false
     "SD - Ganho Peso Automatico Algodao US #145369 - WPP
     AND data_carga       IN lra_data_carga  "SD - Ganho Peso Automatico Algodao US #145369 - WPP
     "Não pode executar estorno do fardos se tive movimento de ajuste de ganho/perda de peso pendente de geração para o romaneio
     AND NOT EXISTS ( SELECT id_carga
                      FROM zsdt0344 AS b
                      WHERE b~id_carga           EQ a~id_carga
                        AND b~seq_carga          EQ a~seq
                        AND b~ch_referencia_rom  EQ a~ch_referencia
                        AND b~mblnr              EQ space ).
  "SD - Ganho Peso Automatico Algodao US #145369 - WPP

  CHECK t_zsdt0330_est[] IS NOT INITIAL.

*-----------------------------
* procura os documentos gerados
*-----------------------------
  SELECT *
    FROM zsdt0330
    INTO TABLE t_zsdt0330
     FOR ALL ENTRIES  IN t_zsdt0330_est
   WHERE id_carga        = t_zsdt0330_est-id_carga
     AND matnr           = t_zsdt0330_est-matnr
     AND werks           = t_zsdt0330_est-werks
     AND lgort           = t_zsdt0330_est-lgort
     AND acharg          = t_zsdt0330_est-acharg
     AND safra           = t_zsdt0330_est-safra
     AND status_estorno <> 'D'
     AND cancelado        = abap_false.

  CHECK t_zsdt0330[] IS NOT INITIAL.

  "Projeto Reestruturação Algodao 2024 - Ini
*  SELECT *
*    FROM mseg
*    INTO TABLE t_mseg
*     FOR ALL ENTRIES IN t_zsdt0330
*   WHERE mblnr = t_zsdt0330-mblnr
*     AND mjahr = t_zsdt0330-mjahr
*     AND charg = t_zsdt0330-acharg
*     AND lgort = t_zsdt0330-lgort
*     AND bwart = 'ZA1'.
*
*  DELETE t_mseg WHERE smbln <> abap_off.
*  SORT t_mseg BY matnr werks charg lgort mblnr DESCENDING .
*  DELETE ADJACENT DUPLICATES FROM t_mseg
*                        COMPARING matnr werks charg lgort.

*  SELECT *
*    FROM zppt0002
*    INTO TABLE t_zppt0002
*     FOR ALL ENTRIES IN t_zsdt0330
*   WHERE cd_sai = t_zsdt0330-cd_sai.
  "Projeto Reestruturação Algodao 2024 - Fim

  "SD - Ganho Peso Automatico Algodao US #145369 - WPP
*  SELECT *
*    FROM zmmt0008
*    INTO TABLE t_zmmt0008
*     FOR ALL ENTRIES IN t_zsdt0330
*   WHERE werks       = t_zsdt0330-werks
*     AND lgort       = t_zsdt0330-lgort
*     AND charg       = t_zsdt0330-acharg
*     AND vbeln       = t_zsdt0330-vbeln
*     AND nr_romaneio = t_zsdt0330-nr_romaneio.
  "SD - Ganho Peso Automatico Algodao US #145369 - WPP

ENDFORM.

************************************************************************
*&-processamento dos estornos dos fardos
************************************************************************
FORM f_processa_estornos.

  CHECK t_zsdt0330[] IS NOT INITIAL.

  LOOP AT t_zsdt0330 INTO w_zsdt0330.
    FREE: t_carga, w_mseg, w_zsdt0330_est, t_fardos_mov.

    READ TABLE t_zsdt0330_est INTO w_zsdt0330_est WITH KEY id_carga = w_zsdt0330-id_carga
                                                           matnr    = w_zsdt0330-matnr
                                                           werks    = w_zsdt0330-werks
                                                           lgort    = w_zsdt0330-lgort
                                                           acharg   = w_zsdt0330-acharg
                                                           safra    = w_zsdt0330-safra.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING w_zsdt0330_est TO w_carga.
    APPEND w_carga                    TO t_carga.

    CHECK w_zsdt0330-mblnr IS NOT INITIAL. "Projeto Reestruturação Algodao 2024

    MESSAGE |Iniciando estorno do fardo { w_zsdt0330-acharg } id carga: { w_zsdt0330-id_carga } | TYPE 'S'.

*    READ TABLE t_mseg INTO w_mseg WITH KEY mblnr = w_zsdt0330-mblnr
*                                           mjahr = w_zsdt0330-mjahr.
*    CHECK sy-subrc = 0.

    "Projeto Reestruturação Algodao 2024
    APPEND VALUE #( nr_fardo_completo = w_zsdt0330-acharg ) TO t_fardos_mov. "Projeto Reestruturação Algodao 2024

    zcl_comercializacao_algodao=>estornar_disp_fardos_comerc( EXPORTING i_safra                 = CONV #( w_zsdt0330-safra )
                                                                        i_filial_algodoeira     = CONV #( w_zsdt0330-werks )
                                                                        i_bloco                 = CONV #( w_zsdt0330-lgort )
                                                                        i_matnr                 = CONV #( w_zsdt0330-matnr )
                                                                        i_fardos_estornar       = t_fardos_mov
                                                                        i_carregamento_auto     = abap_true
                                                              IMPORTING e_msg_sucesso           = DATA(lva_msg_sucess)
                                                                        e_mblnr                 = DATA(lva_mblnr)
                                                                        e_mjahr                 = DATA(lva_mjahr)
                                                                        e_msg_error             = DATA(lva_msg_error)
                                                                        e_error_lock            = DATA(lva_error_lock) ).

    IF lva_msg_error IS NOT INITIAL.

      IF lva_error_lock EQ abap_true.
        PERFORM f_gera_msg_carga_estorno USING '0' '' '' lva_msg_error. "Marcar os fardos com pendente de processamento para novamente tentativa de movimentação, devido o erro de lock
      ELSE.
        PERFORM f_gera_msg_carga_estorno USING '2' '' '' lva_msg_error. "Marcar os fardos com erro de processamento
      ENDIF.

      CONTINUE.
    ELSEIF lva_msg_sucess IS NOT INITIAL.
      PERFORM f_gera_msg_carga_estorno USING '3'
                                              lva_mblnr
                                              lva_mjahr
                                              lva_msg_sucess.
    ENDIF.

    CHECK lva_msg_sucess IS NOT INITIAL.

    MESSAGE |Estorno do fardo { w_zsdt0330-acharg } id carga: { w_zsdt0330-id_carga } realizado com sucesso | TYPE 'S'.

*    PERFORM f_estorna_movto CHANGING l_erro.

*    IF w_mseg-bwart = '261'.
*      PERFORM f_estorna_261   CHANGING l_erro.
*    ELSE.
*      PERFORM f_estorna_movto CHANGING l_erro.
*    ENDIF.

*    CHECK l_erro = abap_false.

*    PERFORM f_estorna_zmmt0008.

    "Projeto Reestruturação Algodao 2024

    PERFORM f_cancela_zsdt0330.

  ENDLOOP.


ENDFORM.

************************************************************************
*&-processamento dos fardos
************************************************************************
FORM f_processa_dados.

  DATA: lit_return TYPE TABLE OF bapiret2.

*------------------------------------------
*- selecao dos fardos a processar
*------------------------------------------
  IF p_unico = abap_true.
    SELECT *
      FROM zsdt0330
      INTO TABLE t_zsdt0330
     WHERE id_carga         = p_idcar
       AND matnr            = p_matnr
       AND werks            = p_werks
       AND lgort            = p_lgort
       AND acharg           = p_acharg
       AND safra            = p_safra
       AND status_fardo     = '1'
       AND status_estorno  IN (abap_false,'I')
       AND cancelado        = abap_false.
  ELSE.
    SELECT *
      FROM zsdt0330
      INTO TABLE t_zsdt0330
     WHERE chave_referencia = p_ch_ref
       AND status_fardo     = '1'
       AND status_estorno  IN (abap_false,'I')
       AND cancelado        = abap_false
       AND data_carga    IN lra_data_carga.  "SD - Ganho Peso Automatico Algodao US #145369 - WPP
  ENDIF.

  DELETE t_zsdt0330 WHERE ch_referencia IS INITIAL. "Só iniciar movimentação dos fardos se romaneio já estiver no SAP - SD - Ganho Peso Automatico Algodao US #145369 - WPP


  CHECK t_zsdt0330[] IS NOT INITIAL.

*------------------------------------------
*- checa se carga esta completa
*------------------------------------------
  t_0330_carga[] = t_zsdt0330[].

  SORT t_0330_carga BY id_carga.
  DELETE ADJACENT DUPLICATES FROM t_0330_carga
                        COMPARING id_carga.

  LOOP AT t_0330_carga INTO w_0330_carga.
    l_tabix = sy-tabix.

    CLEAR l_fardos.

    SELECT COUNT(*)
      FROM zsdt0330
      INTO l_fardos
     WHERE id_carga  = w_0330_carga-id_carga
       AND cancelado = abap_false.

    IF w_0330_carga-qtd_fardos_carga <> l_fardos AND
       w_0330_carga-status_estorno    = abap_false.
      SELECT *
        INTO TABLE @DATA(t_0330)
        FROM zsdt0330
       WHERE id_carga  = @w_0330_carga-id_carga
         AND cancelado = @abap_false.

      MOVE-CORRESPONDING t_0330[] TO t_carga[].

      l_mensagem = 'Carregamento está incompleto. Faltam Fardinhos!'.
      PERFORM f_gera_erro_carga USING '0' l_mensagem.

      LOOP AT t_zsdt0330 INTO w_zsdt0330 WHERE id_carga = w_0330_carga-id_carga.
        DELETE t_zsdt0330 INDEX sy-tabix.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

*------------------------------------------
*- inicio processamrnto
*------------------------------------------
  t_0330_carga[] = t_zsdt0330[].

  SORT t_0330_carga BY id_carga matnr werks lgort safra.
  DELETE ADJACENT DUPLICATES FROM t_0330_carga
                        COMPARING id_carga matnr werks lgort safra.

  LOOP AT t_0330_carga INTO w_0330_carga.

    FREE: t_carga, l_mensagem, t_fardos_mov.

    LOOP AT t_zsdt0330            INTO w_zsdt0330 WHERE id_carga = w_0330_carga-id_carga
                                                    AND matnr    = w_0330_carga-matnr
                                                    AND werks    = w_0330_carga-werks
                                                    AND lgort    = w_0330_carga-lgort
                                                    AND safra    = w_0330_carga-safra.
      MOVE-CORRESPONDING w_zsdt0330 TO w_carga.
      APPEND w_carga                TO t_carga.
      APPEND VALUE #( nr_fardo_completo = w_zsdt0330-acharg ) TO t_fardos_mov. "Projeto Reestruturação Algodao 2024


*     MOVE '1'                      TO w_zsdt0330-status_fardo.
*     MODIFY zsdt0330             FROM w_zsdt0330.
    ENDLOOP.

*   COMMIT WORK.

*-------------------------------------------
* busca deposito destino
*-------------------------------------------
    SELECT SINGLE lgort_destino
             INTO @DATA(l_lgort_destino)
             FROM zsdt0332
            WHERE werks             = @w_0330_carga-werks
              AND safra             = @w_0330_carga-safra
              AND lgort_origem_ini <= @w_0330_carga-lgort
              AND lgort_origem_fim >= @w_0330_carga-lgort.

    IF sy-subrc <> 0.
      l_mensagem   = 'Deposito destino não Encontrado.'.
      PERFORM f_gera_erro_carga USING '2' l_mensagem.
      CONTINUE.
    ENDIF.

    "Projeto Reestruturação Algodao 2024
    zcl_comercializacao_algodao=>disp_fardos_comercializar( EXPORTING i_safra                 = CONV #( w_0330_carga-safra )
                                                                      i_filial_algodoeira     = CONV #( w_0330_carga-werks )
                                                                      i_bloco                 = CONV #( w_0330_carga-lgort )
                                                                      i_matnr                 = CONV #( w_0330_carga-matnr )
                                                                      i_lgort_destino         = CONV #( l_lgort_destino )
                                                                      i_fardos_disponibilizar = t_fardos_mov
                                                                      i_carregamento_auto     = abap_true
                                                            IMPORTING e_msg_sucesso           = DATA(lva_msg_sucess)
                                                                      e_mblnr                 = DATA(lva_mblnr)
                                                                      e_mjahr                 = DATA(lva_mjahr)
                                                                      e_msg_error             = DATA(lva_msg_error)
                                                                      e_error_lock            = DATA(lva_error_lock) ).

    IF lva_msg_error IS NOT INITIAL.

      IF lva_error_lock EQ abap_true.
        PERFORM f_gera_erro_carga USING '0' lva_msg_error. "Marcar os fardos com pendente de processamento para novamente tentativa de movimentação, devido o erro de lock
      ELSE.
        PERFORM f_gera_erro_carga USING '2' lva_msg_error. "Marcar os fardos com erro de processamento
      ENDIF.

      CONTINUE.
    ELSEIF lva_msg_sucess IS NOT INITIAL.
      PERFORM f_gera_sucesso_carga USING lva_mblnr
                                         lva_mjahr
                                         l_lgort_destino
                                         lva_msg_sucess.
    ENDIF.

*    PERFORM f_selecao_dados CHANGING l_erro
*                                     l_mensagem.
*
*    IF l_erro = abap_true.
*      PERFORM f_gera_erro_carga USING '2' l_mensagem.
*      CONTINUE.
*    ENDIF.
*
*    PERFORM f_monta_mchb.
*    PERFORM f_gerar_transferencia.
    "Projeto Reestruturação Algodao 2024

  ENDLOOP.

ENDFORM.



************************************************************************
*&-processamento dos fardos
************************************************************************
FORM f_ajusta_movimento_sobra_perda.

  DATA: lit_zsdt0344 TYPE TABLE OF zsdt0344.

  CHECK p_unico = abap_false.

  "Levantar Manutençao de carga que estao pendente de geração de movimento de ganho/perda
  SELECT *
   FROM zsdt0344 AS a  INTO TABLE lit_zsdt0344
  WHERE chave_referencia EQ p_ch_ref
    AND dt_registro      IN lra_data_carga
    AND mblnr            EQ space.

  DELETE lit_zsdt0344 WHERE ch_referencia_rom IS INITIAL.

  SORT lit_zsdt0344 BY id_carga ch_referencia_rom.
  DELETE ADJACENT DUPLICATES FROM lit_zsdt0344 COMPARING id_carga ch_referencia_rom.

  LOOP AT lit_zsdt0344 ASSIGNING FIELD-SYMBOL(<fs_zsdt0344>).

    zcl_comercializacao_algodao=>ck_ajuste_sobra_perda_romaneio(
      EXPORTING
        i_id_carga      = <fs_zsdt0344>-id_carga
        i_ch_referencia = <fs_zsdt0344>-ch_referencia_rom
      IMPORTING
        e_msg_sucesso   = DATA(_msg_sucesso)
        e_mblnr         = DATA(_mblnr)
        e_mjahr         = DATA(mjahr)
        e_msg_error     = DATA(_msg_error)  ).

  ENDLOOP.


ENDFORM.


************************************************************************
*&-selecao dados
************************************************************************
*FORM f_selecao_dados CHANGING p_erro
*                              p_mensagem.
*
*  FREE: p_erro, p_mensagem.
*
*  PERFORM: f_seleciona_mchb CHANGING p_erro
*                                     p_mensagem,
*           f_seleciona_makt.
*
*ENDFORM.

************************************************************************
*&-selecao mchb
************************************************************************
*FORM f_seleciona_mchb CHANGING p_erro
*                               p_mensagem.
*
*  FREE: t_mchb, r_charg, p_erro, p_mensagem.
*
*  READ TABLE t_carga INTO w_carga INDEX 1.
*
*  SELECT matnr werks lgort
*         charg clabs cspem
*    FROM mchb
*    INTO TABLE t_mchb
*     FOR ALL ENTRIES IN t_carga
*   WHERE matnr  = t_carga-matnr
*     AND werks  = t_carga-werks
*     AND lgort  = t_carga-lgort
*     AND charg  = t_carga-acharg.
*
**------------------------------------------
** Selecionar cd sai.
**------------------------------------------
*  IF t_mchb[] IS NOT INITIAL.
*    SELECT SINGLE *            "Consulta SET Safra.
*      FROM setleaf
*      INTO @DATA(i_data)
*     WHERE setname EQ 'MAGGI_ACTS_ZMM0023'.
*
*    SELECT *
*      FROM zppt0002
*      INTO TABLE t_zppt0002
*       FOR ALL ENTRIES IN t_mchb
*     WHERE acharg    = t_mchb-charg
*       AND cd_safra  = i_data-valfrom.
*
*    IF sy-subrc EQ 0.
*      r_charg = VALUE #( FOR l IN t_zppt0002 ( sign = 'I' option = 'EQ' low = l-acharg  ) ).
*    ELSE.
*      v_trace = 1.
*    ENDIF.
*  ENDIF.
*
*  IF v_trace = '0'.
**   CHECK r_charg[] IS NOT INITIAL.
**   DELETE t_mchb WHERE charg NOT IN r_charg.
**   CHECK t_mchb[]  IS NOT INITIAL.
*    IF r_charg[] IS NOT INITIAL.
*      DELETE t_mchb WHERE charg NOT IN r_charg.
*      IF t_mchb[] IS INITIAL.
*        p_erro      = abap_true.
*        p_mensagem  = 'Nenhum Estoque de Lote Encontrado.'.
*        EXIT.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  CLEAR: vg_qtde.
*  DESCRIBE TABLE t_mchb LINES vg_qtde.
*
**------------------------------------------
** Consulta da API.
**------------------------------------------
*  TRY .
*      CLEAR: vg_werk, vg_cd_sai, vg_lote, vg_lgort.
*      vg_werk   = w_carga-werks.
**     vg_cd_sai = w_carga-cd_sai.
*      vg_lote   = w_carga-lgort.
*      vg_lgort  = w_carga-safra.
*
*      LOOP AT t_mchb ASSIGNING FIELD-SYMBOL(<sl_mchb>).
*
*        IF v_letrac = 0 AND v_trace = 0.
*          zcl_int_acts_tracecotton=>zif_int_acts_tracecotton~get_instance( i_servico =  '23'
*         )->set_fazenda(    EXPORTING i_fazenda = vg_werk
*         )->set_lote(       EXPORTING i_lote    = vg_lgort
*         )->set_lgort(      EXPORTING i_lgort   = vg_lote
*         )->set_cd_sai(     EXPORTING i_cd_sai  = vg_cd_sai
*         )->set_dados_acts( IMPORTING e_return  = r_data ).
*          v_letrac = 1.
*        ENDIF.
*
*        l_tabix = sy-tabix.
*
*        READ TABLE t_zppt0002 INTO DATA(ws_zppt0002) WITH KEY acharg = <sl_mchb>-charg.
*        IF sy-subrc = 0.
*          <sl_mchb>-cd_sai = ws_zppt0002-cd_sai.
*
*          READ TABLE r_data INTO DATA(w_data) WITH KEY codigosai = ws_zppt0002-cd_sai.
*
*          IF sy-subrc = 0.
*            IF w_data-takeupmarcadoacts EQ abap_true.
*              IF w_data-possuiacts EQ abap_true AND w_data-statustakeuploterecente EQ 'Aprovado'.
*                <sl_mchb>-icon    = icon_led_green.
*                <sl_mchb>-status  = w_data-statustakeuploterecente.
*                <sl_mchb>-possuiacts  = w_data-possuiacts.
*
*              ELSEIF w_data-possuiacts EQ abap_false AND w_data-statustakeuploterecente NE 'Aprovado'.
*                <sl_mchb>-icon   = icon_led_red.
*                <sl_mchb>-status = w_data-statustakeuploterecente.
*                <sl_mchb>-possuiacts  = w_data-possuiacts.
*
*              ELSEIF w_data-possuiacts EQ abap_true AND w_data-statustakeuploterecente NE 'Aprovado'.
*                <sl_mchb>-icon    = icon_led_red.
*                <sl_mchb>-status  = w_data-statustakeuploterecente.
*                <sl_mchb>-possuiacts  = w_data-possuiacts.
*
*              ELSEIF w_data-possuiacts EQ abap_false AND w_data-statustakeuploterecente EQ 'Aprovado'.
*                <sl_mchb>-icon    = icon_led_red.
*                <sl_mchb>-status  = w_data-statustakeuploterecente.
*                <sl_mchb>-possuiacts  = w_data-possuiacts.
*              ENDIF.
*
*            ELSE.
*
*              IF w_data-statustakeuploterecente EQ 'Aprovado'.
*                <sl_mchb>-icon   = icon_led_green.
*                <sl_mchb>-status = w_data-statustakeuploterecente.
*                <sl_mchb>-possuiacts  = abap_false.
*
*              ELSEIF w_data-statustakeuploterecente NE 'Aprovado'.
*                <sl_mchb>-icon   = icon_led_red.
*                <sl_mchb>-status = w_data-statustakeuploterecente.
*                <sl_mchb>-possuiacts  = abap_false.
*
*              ELSE.
*                <sl_mchb>-icon   = icon_led_red.
*                <sl_mchb>-status = abap_false.
*                <sl_mchb>-possuiacts  = abap_false.
*              ENDIF.
*            ENDIF.
*
*          ELSE.
*            <sl_mchb>-icon   = icon_led_red.
*            <sl_mchb>-status = abap_false.
*            <sl_mchb>-possuiacts  = abap_false.
*          ENDIF.
*
*        ELSE.
*          IF v_trace EQ '1'.
*            <sl_mchb>-icon   = icon_led_green.
*            <sl_mchb>-status = w_data-statustakeuploterecente.
*            <sl_mchb>-possuiacts  = abap_false.
*
*            IF <sl_mchb>-clabs IS INITIAL AND <sl_mchb>-cspem IS NOT INITIAL.
*              <sl_mchb>-clabs = <sl_mchb>-cspem.
*              MODIFY t_mchb FROM <sl_mchb> INDEX l_tabix.
*            ENDIF.
*          ELSE.
*            <sl_mchb>-icon   = icon_led_red.
*            <sl_mchb>-status = abap_false.
*            <sl_mchb>-possuiacts  = abap_false.
*          ENDIF.
*        ENDIF.
*
*        IF <sl_mchb>-clabs IS INITIAL AND v_trace EQ 0.
*          <sl_mchb>-clabs = sl_mchb-cspem.
*        ENDIF.
*
*        CLEAR: w_data, ws_zppt0002.
*      ENDLOOP.
*
*    CATCH zcx_integracao INTO DATA(ex_integra).    "
*      <sl_mchb>-icon       = icon_led_red.
*      <sl_mchb>-status     = abap_false.
*      <sl_mchb>-possuiacts = abap_false.
*      p_erro               = abap_true.
*      p_mensagem           = 'Falha de Comunicação ao buscar ACTS'.
*
*    CATCH zcx_error INTO DATA(ex_error).    "  "
*      <sl_mchb>-icon       = icon_led_red.
*      <sl_mchb>-status     = abap_false.
*      <sl_mchb>-possuiacts = abap_false.
*      p_erro               = abap_true.
*      p_mensagem           = 'Falha de Comunicação ao buscar ACTS'.
*  ENDTRY.
*
**-------------------------------------------
** validar clabs
**-------------------------------------------
*  DELETE t_mchb WHERE clabs = 0.
*  SORT t_mchb BY matnr ASCENDING
*                 werks ASCENDING
*                 lgort ASCENDING
*                 charg ASCENDING.
*
*  IF t_mchb[] IS INITIAL.
*    p_erro               = abap_true.
*    p_mensagem           = 'Nenhum Estoque de Lote Encontrado.'.
*    MESSAGE s836 WITH TEXT-016 DISPLAY LIKE 'I'.
*    EXIT.
*  ENDIF.
*
**-------------------------------------------
** busca deposito destino
**-------------------------------------------
*  CLEAR sl_mchb-lgortr.
*
*  SELECT SINGLE lgort_destino
*           INTO sl_mchb-lgortr
*           FROM zsdt0332
*          WHERE werks             = w_carga-werks
*            AND safra             = w_carga-safra
*            AND lgort_origem_ini <= w_carga-lgort
*            AND lgort_origem_fim >= w_carga-lgort.
*
*  sl_mchb-chargr = w_carga-safra.
*  sl_mchb-matnc  = abap_off.
*
*  MODIFY t_mchb FROM sl_mchb TRANSPORTING lgortr chargr matnc WHERE lgortr = space.
*
*ENDFORM.

************************************************************************
*&-selecao makt
************************************************************************
*FORM f_seleciona_makt.
*
*  DATA: tl_mchb TYPE TABLE OF type_mchb.
*
*  REFRESH t_makt.
*
*  CHECK NOT t_mchb[] IS INITIAL.
*  tl_mchb[] = t_mchb[].
*
*  SORT tl_mchb BY matnr ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM tl_mchb COMPARING matnr.
*
*  SELECT matnr maktx
*    FROM makt
*    INTO TABLE t_makt
*     FOR ALL ENTRIES IN tl_mchb
*   WHERE matnr EQ tl_mchb-matnr
*     AND spras EQ sy-langu.
*
*  SORT t_makt BY matnr ASCENDING.
*
*ENDFORM.                    " Z_SELECIONA_MAKT

************************************************************************
*&-monta mchb
************************************************************************
FORM f_monta_mchb.

  DATA: sl_makt  TYPE type_makt,
        sl_mchb  TYPE type_mchb,
        vl_index TYPE sy-tabix.

  LOOP AT t_mchb INTO sl_mchb.
    vl_index = sy-tabix.

    READ TABLE t_makt INTO sl_makt WITH KEY matnr = sl_mchb-matnr
                                   BINARY SEARCH.
    sl_mchb-maktx = sl_makt-maktx.

    MODIFY t_mchb FROM sl_mchb INDEX vl_index TRANSPORTING maktx.

    CLEAR: sl_makt,
           sl_mchb.
  ENDLOOP.

ENDFORM.                    " Z_PROCESSA_DADOS

************************************************************************
*&-ajusta zsdt0331
************************************************************************
FORM f_ajusta_taberros.

  LOOP AT t_carga  INTO w_carga.
    UPDATE zsdt0331 SET cancelado    = abap_true
                  WHERE id_carga     = w_carga-id_carga
                    AND matnr        = w_carga-matnr
                    AND werks        = w_carga-werks
                    AND lgort        = w_carga-lgort
                    AND cd_sai       = w_carga-cd_sai
                    AND safra        = w_carga-safra.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

************************************************************************
*&-gerar erro para o fardo
************************************************************************
FORM f_gerar_transferencia.

  DATA: sl_trans    TYPE type_mchb,
        sl_header   TYPE bapi2017_gm_head_01,
        vl_code     TYPE bapi2017_gm_code,
        vl_material TYPE bapi2017_gm_head_ret-mat_doc,
        vl_year     TYPE bapi2017_gm_head_ret-doc_year,
        tl_item     TYPE TABLE OF bapi2017_gm_item_create,
        tl_return   TYPE TABLE OF bapiret2,
        sl_return   TYPE bapiret2,
        sl_item     TYPE bapi2017_gm_item_create,
        r_data      TYPE zde_acts_tracecotton_t,
        i_date      TYPE bapi2017_gm_head_ret-doc_year,
        i_doc_mat   TYPE bapi2017_gm_head_ret-mat_doc,
        sl_zmmt0008 TYPE zmmt0008.

  FREE: t_trans, t_msn, sl_header.

  LOOP AT t_mchb  INTO sl_trans.
    APPEND sl_trans TO t_trans.
  ENDLOOP.

  SORT t_trans BY matnr ASCENDING
                  werks ASCENDING
                  lgort ASCENDING
                  charg ASCENDING.

  READ TABLE t_trans INTO DATA(ws_trans) WITH KEY icon = icon_led_red.
  IF sy-subrc = 0.
    l_mensagem = 'Existem lotes com status diferente de aprovado ou não possui ACTS, verificar!'.
    PERFORM f_gera_erro_carga    USING '2' l_mensagem.
*   PERFORM f_envia_trace_cotton USING     l_mensagem.
    EXIT.
  ELSE.
*   PERFORM f_ajusta_taberros.
  ENDIF.

  READ TABLE t_carga INTO w_carga INDEX 1.

*---------------------------------------
* efetua movto mercadoria
*---------------------------------------
  LOOP AT t_trans INTO sl_trans.

    FREE: vl_material, vl_year, sl_zmmt0008, tl_item, tl_return.

    IF sy-tabix EQ 1.
      vl_code              = '06'.
      sl_header-pstng_date = sy-datum.
      sl_header-doc_date   = sy-datum.
    ENDIF.

    sl_item-move_type      = 'ZA1'.
    sl_item-material       = sl_trans-matnr.
    sl_item-plant          = sl_trans-werks.
    sl_item-stge_loc       = sl_trans-lgort.
    sl_item-batch          = sl_trans-charg.
    sl_item-entry_qnt      = sl_trans-clabs.
    sl_item-move_plant     = sl_trans-werks.
    sl_item-move_stloc     = sl_trans-lgortr.
    sl_item-move_mat       = sl_trans-matnc.
*   sl_item-customer       = p_kunnr.
*   sl_header-header_txt   = p_sgtxt.

    IF NOT sl_trans-chargr IS INITIAL.
      sl_item-move_batch   = sl_trans-chargr.
    ELSE.
      sl_item-move_batch   = sl_trans-charg.
    ENDIF.

    APPEND sl_item TO tl_item.

*-----------------------------------------
*-- executa bapi, verificando bloqueio de fardo
*-----------------------------------------
    DO 5 TIMES.
      FREE: tl_return, l_erro_lock, is_block.

      l_index = sy-index.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = sl_header
          goodsmvt_code    = vl_code
        IMPORTING
          materialdocument = vl_material
          matdocumentyear  = vl_year
        TABLES
          goodsmvt_item    = tl_item
          return           = tl_return.

      READ TABLE tl_return INTO sl_return WITH KEY type = 'E'.

      IF sy-subrc = 0.
        CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
          EXPORTING
            id       = sl_return-id
            number   = sl_return-number
          IMPORTING
            is_block = is_block.

        l_erro_lock = is_block.

        IF is_block IS NOT INITIAL AND l_index NE 5.
          WAIT UP TO 3 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    IF l_erro_lock = abap_true.
      PERFORM f_monta_erro TABLES tl_return
                            USING sl_trans-matnr
                                  sl_trans-werks
                                  sl_trans-lgort
                                  sl_trans-lgortr
                                  sl_trans-charg
                                  vl_material
                                  vl_year
                                  '0'.
      CONTINUE.
    ENDIF.

    IF NOT vl_material IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      FREE tl_return.

      sl_return-type     = 'S'.
      CONCATENATE TEXT-013
                  vl_material
                  TEXT-014
                  vl_year
                  TEXT-015
             INTO sl_return-message SEPARATED BY space.
      APPEND sl_return TO tl_return.

      PERFORM f_monta_erro TABLES tl_return
                            USING sl_trans-matnr
                                  sl_trans-werks
                                  sl_trans-lgort
                                  sl_trans-lgortr
                                  sl_trans-charg
                                  vl_material
                                  vl_year
                                  '3'.

      IF sl_item-move_type EQ 'ZA1'.
        sl_zmmt0008-werks  = sl_trans-werks.
        sl_zmmt0008-lgort  = sl_trans-lgort.
        sl_zmmt0008-charg  = sl_trans-charg.
        sl_zmmt0008-menge  = sl_trans-clabs.
        sl_zmmt0008-matnr  = sl_trans-matnr.    "*-S4H-US 123905-25.09.2023-JT
        sl_zmmt0008-lgortr = sl_trans-lgortr.   "*-S4H-US 123905-25.09.2023-JT
        sl_zmmt0008-safra  = sl_trans-chargr.    "*-S4H-US 123905-25.09.2023-JT
        sl_zmmt0008-mblnr  = vl_material.       "*-S4H-US 123905-25.09.2023-JT
        sl_zmmt0008-mjahr  = vl_year.           "*-S4H-US 123905-25.09.2023-JT
        MODIFY zmmt0008 FROM sl_zmmt0008.
      ENDIF.

      DELETE t_trans WHERE matnr EQ sl_trans-matnr
                       AND werks EQ sl_trans-werks
                       AND lgort EQ sl_trans-lgort
                       AND charg EQ sl_trans-charg.
      DELETE t_mchb  WHERE matnr EQ sl_trans-matnr
                       AND werks EQ sl_trans-werks
                       AND lgort EQ sl_trans-lgort
                       AND charg EQ sl_trans-charg.

    ELSE.
*     Retorna Erro
      PERFORM f_monta_erro TABLES tl_return
                            USING sl_trans-matnr
                                  sl_trans-werks
                                  sl_trans-lgort
                                  sl_trans-lgortr
                                  sl_trans-charg
                                  vl_material
                                  vl_year
                                  '2'.
    ENDIF.

    CLEAR: sl_trans,
           sl_item.

  ENDLOOP.

ENDFORM.

************************************************************************
*&-monta saida de erro
************************************************************************
FORM f_monta_erro TABLES p_return  STRUCTURE bapiret2
                   USING p_matnr
                         p_werks
                         p_lgort
                         p_lgort_rec
                         p_charg
                         p_mat_doc TYPE bapi2017_gm_head_ret-mat_doc
                         p_year    TYPE bapi2017_gm_head_ret-doc_year
                         p_status.

  DATA: sl_return TYPE bapiret2,
        sl_msn    TYPE type_msn.

  LOOP AT p_return INTO sl_return.
    l_mensagem  = sl_return-message.

    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_log( EXPORTING i_id_carga  = w_carga-id_carga
                                    i_matnr     = p_matnr
                                    i_werks     = p_werks
                                    i_lgort     = p_lgort
                                    i_acharg    = p_charg
                                    i_safra     = w_carga-safra
                                    i_tipo_msg  = COND #( WHEN p_mat_doc IS INITIAL THEN 'E'
                                                                                    ELSE 'S' )
                                    i_mensagem  = l_mensagem ).
  ENDLOOP.

  IF p_mat_doc IS NOT INITIAL.
    UPDATE zsdt0330 SET mblnr        = p_mat_doc
                        mjahr        = p_year
                        lgort_rec    = p_lgort_rec
                        status_fardo = '3'
                  WHERE id_carga     = w_carga-id_carga
                    AND matnr        = p_matnr
                    AND werks        = p_werks
                    AND lgort        = p_lgort
                    AND acharg       = p_charg
                    AND safra        = w_carga-safra
                    AND cancelado    = abap_off.
  ELSE.
    UPDATE zsdt0330 SET status_fardo = p_status
                        lgort_rec    = p_lgort_rec
                  WHERE id_carga     = w_carga-id_carga
                    AND matnr        = p_matnr
                    AND werks        = p_werks
                    AND lgort        = p_lgort
                    AND acharg       = p_charg
                    AND safra        = w_carga-safra
                    AND cancelado    = abap_off.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.                    " Z_MONTA_ERRO

************************************************************************
*&-gerar erro para o fardo
************************************************************************
FORM f_gera_erro_carga USING p_status
                             p_mensagem.

  LOOP AT t_carga INTO w_carga.
    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_log( EXPORTING i_id_carga  = w_carga-id_carga
                                    i_matnr     = w_carga-matnr
                                    i_werks     = w_carga-werks
                                    i_lgort     = w_carga-lgort
                                    i_acharg    = w_carga-acharg
                                    i_safra     = w_carga-safra
                                    i_tipo_msg  = 'E'
                                    i_mensagem  = p_mensagem ).

    UPDATE zsdt0330 SET status_fardo = p_status
                  WHERE id_carga     = w_carga-id_carga
                    AND matnr        = w_carga-matnr
                    AND werks        = w_carga-werks
                    AND lgort        = w_carga-lgort
                    AND acharg       = w_carga-acharg
                    AND safra        = w_carga-safra
                    AND cancelado    = abap_false.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

FORM f_gera_sucesso_carga USING p_mat_doc TYPE bapi2017_gm_head_ret-mat_doc
                                p_year    TYPE bapi2017_gm_head_ret-doc_year
                                p_lgort_rec
                                p_mensagem.

  LOOP AT t_carga INTO w_carga.
    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_log( EXPORTING i_id_carga  = w_carga-id_carga
                                    i_matnr     = w_carga-matnr
                                    i_werks     = w_carga-werks
                                    i_lgort     = w_carga-lgort
                                    i_acharg    = w_carga-acharg
                                    i_safra     = w_carga-safra
                                    i_tipo_msg  = 'S'
                                    i_mensagem  = p_mensagem ).

    UPDATE zsdt0330 SET mblnr        = p_mat_doc
                        mjahr        = p_year
                        lgort_rec    = p_lgort_rec
                        status_fardo = '3'
                  WHERE id_carga     = w_carga-id_carga
                    AND matnr        = w_carga-matnr
                    AND werks        = w_carga-werks
                    AND lgort        = w_carga-lgort
                    AND acharg       = w_carga-acharg
                    AND safra        = w_carga-safra
                    AND seq          = w_carga-seq    "SD - Ganho Peso Automatico Algodao US #145369 - WPP
                    AND cancelado    = abap_false.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

************************************************************************
*&-enviar erro trace cotton
************************************************************************
FORM f_envia_trace_cotton USING p_mensagem.

  LOOP AT  t_trans INTO DATA(w_trans) WHERE icon = icon_led_red.

    CLEAR w_retorno.

    READ TABLE t_carga INTO DATA(_carga) WITH KEY matnr  = w_trans-matnr
                                                  werks  = w_trans-werks
                                                  lgort  = w_trans-lgort
                                                  acharg = w_trans-charg.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING _carga      TO w_retorno.
    MOVE                p_mensagem TO w_retorno-mensagem.

    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_retorno( w_retorno ).

  ENDLOOP.

  COMMIT WORK.

*--------------------------------------
*-- envio de retorno de erros ao trace
*--------------------------------------
  TRY .
      zcl_trace_cotton=>zif_trace_cotton~get_instance(
         )->set_retorno_trace( i_id_carga = w_carga-id_carga ).

    CATCH zcx_integracao INTO DATA(ex_integra).
    CATCH zcx_error INTO DATA(ex_error).
  ENDTRY.

  COMMIT WORK.

ENDFORM.

************************************************************************
* estorna movimento 261
************************************************************************
*FORM f_estorna_261  CHANGING p_erro.
*
*  DATA: is_block TYPE char01,
*        vl_index TYPE sy-index.
*
*  CLEAR: es_bflushflags, es_bflushdatagen, p_erro.
*  REFRESH: it_goodsmovements.
*
*  READ TABLE t_zppt0002 INTO w_zppt0002 WITH KEY cd_sai = w_zsdt0330-cd_sai.
*  CHECK sy-subrc = 0.
*
*  SELECT SINGLE cslid
*    INTO eselid
*    FROM t399d
*   WHERE werks = w_zsdt0330-werks.
*
*  REFRESH: mdpmx, l_plaf.
*
*  l_plaf-matnr = w_zppt0002-matnr.
*  l_plaf-plwrk = w_zppt0002-werks.
*  l_plaf-pwwrk = w_zppt0002-werks.
*  l_plaf-gsmng = w_zppt0002-menge.
*  l_plaf-psttr = w_zppt0002-budat.
*  l_plaf-verid = w_zppt0002-verid.
*  APPEND l_plaf.
*
*  CALL FUNCTION 'MD_AUFLOESUNG_PLANAUFTRAG'
*    EXPORTING
*      eplaf         = l_plaf   " Planauftrag
*      emt61d        = mt61d    " Matstammview
*      eselid        = eselid          " SelektionsID Stüli/ DispWerkTab
*      ecm61m        = l_cm61m  " CO-Bereich Materialartenebene
*      eno_scrap     = xscrap "VGA
*    IMPORTING
*      iplaf         = l_plaf
*    TABLES
*      mdpmx         = mdpmx    " View auf Matkomponenten im Pauf
*    EXCEPTIONS
*      error_message = 1.
*
*  SELECT SINGLE *
*    FROM mkal
*    INTO wa_mkal
*   WHERE werks = w_zppt0002-werks
*     AND matnr = w_zppt0002-matnr
*     AND verid = w_zppt0002-verid.
*
*  es_bflushflags-bckfltype        = '01'.
*  es_bflushdatagen-postdate       = w_zppt0002-budat.
*  es_bflushdatagen-docdate        = w_zppt0002-bldat.
*  es_bflushdatagen-prodplant      = w_zppt0002-werks.      "'Centro (WERKS)'
*
*  IF strlen( w_zppt0002-matnr ) > 18.
*    es_bflushdatagen-materialnr_long = w_zppt0002-matnr.   "'Material Consumo (MATNR) - LONG
*  ELSE.
*    es_bflushdatagen-materialnr      = w_zppt0002-matnr.   "'Material Consumo (MATNR)
*  ENDIF.
*
*  es_bflushdatagen-backflquant    = w_zppt0002-menge.      "'Quantidade (ERFMG)'
*  es_bflushdatagen-unitofmeasure  = 'KG'.                       "'UM (ERFME)'
*  es_bflushdatagen-prodversion    = w_zppt0002-verid.      "'Versão (VERID)'
*  es_bflushdatagen-batch          = w_zppt0002-charg.      "'Lote (ACHARG)'
*  IF wa_mkal-elpro IS NOT INITIAL.
*    es_bflushdatagen-storageloc     = wa_mkal-elpro.              "'Deposito (ALORT)'
*  ELSE.
*    es_bflushdatagen-storageloc     = wa_mkal-alort.              "'Deposito (ALORT)'
*  ENDIF.
*
*  LOOP AT mdpmx.
*    IF strlen( mdpmx-matnr ) > 18.
*      wa_goodsmovements-material_long = mdpmx-matnr.   "'Material Consumo (MATNR) - LONG
*    ELSE.
*      wa_goodsmovements-material      = mdpmx-matnr.   "'Material Consumo (MATNR)
*    ENDIF.
*
*    wa_goodsmovements-plant         = w_zsdt0330-werks.   "'Centro (WERKS)'
*    wa_goodsmovements-stge_loc      = mdpmx-lgpro.             "'Deposito Consumo (LGORT)'
*    IF mdpmx-xchpf = 'X'. "lote obrigatório
*      wa_goodsmovements-batch         = w_zppt0002-charg.   "'Lote Consumo (CHARG)'
*    ENDIF.
*    IF mdpmx-shkzg = 'S'.
*      wa_goodsmovements-move_type     = '531'.
*      wa_goodsmovements-batch = w_zppt0002-cd_safra. "ALRS 19/07/2016
*      IF mdpmx-matnr = '000000000000120166'.
*        IF w_zppt0002-peso_fibrilha GT 0.
*          mdpmx-erfmg = w_zppt0002-peso_fibrilha.
*        ENDIF.
*      ELSEIF mdpmx-matnr = '000000000000120168'.
*        IF w_zppt0002-peso_caroco GT 0.
*          mdpmx-erfmg = w_zppt0002-peso_caroco.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      wa_goodsmovements-move_type     = '261'.
*      vsaldo = 0.
*
*      SELECT SINGLE clabs
*        FROM mchb
*        INTO vsaldo
*       WHERE matnr = mdpmx-matnr
*         AND werks = w_zppt0002-werks
*         AND lgort = mdpmx-lgpro
*         AND charg = w_zppt0002-charg.
*
*      mdpmx-erfmg = vsaldo.
*    ENDIF.
*
*    wa_goodsmovements-entry_qnt     = mdpmx-erfmg.             "'Quantidade consumo (ERFME_R)'
*    wa_goodsmovements-entry_uom     = mdpmx-erfme.             "'UM consumo (ERFME)'
*
*    APPEND wa_goodsmovements TO it_goodsmovements.
*  ENDLOOP.
*
*  CLEAR: fg_bloqueio, sy-msgv1, sperr_user.
*
*  DO 3 TIMES.
*
*    FREE: it_return, wa_return, is_block, p_erro.
*
*    vl_index = sy-index.
*
*    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'
*      EXPORTING
*        bflushflags    = es_bflushflags
*        bflushdatagen  = es_bflushdatagen
**       BFLUSHDATAMTS  =
*      IMPORTING
*        confirmation   = es_confirmation
*        return         = wa_return
*      TABLES
*        goodsmovements = it_goodsmovements.
*
*    APPEND wa_return TO it_return.
*
*    IF wa_return-type = 'E'.
*      p_erro = abap_true.
*
*      CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
*        EXPORTING
*          id       = wa_return-id
*          number   = wa_return-number
*        IMPORTING
*          is_block = is_block.
*
*      l_erro_lock = is_block.
*
*      IF is_block IS NOT INITIAL AND vl_index NE 3.
*        WAIT UP TO 2 SECONDS.
*      ELSE.
*        EXIT.
*      ENDIF.
*    ELSE.
*      EXIT.
*    ENDIF.
*  ENDDO.
*
*  IF p_erro = abap_true.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*    l_mensagem = wa_return-message.
*    PERFORM f_gera_msg_carga_estorno USING '2'
*                                           ''
*                                           ''
*                                           l_mensagem.
*    EXIT.
*  ENDIF.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = abap_true.
*
*  SELECT SINGLE *
*    INTO gs_blpp
*    FROM blpp
*   WHERE prtnr = es_confirmation
*     AND prtps = '0001'.
*
*  WAIT UP TO 2 SECONDS.
*
*  SELECT SINGLE * FROM mseg
*    INTO @DATA(wa_mseg)
*   WHERE mblnr EQ @gs_blpp-belnr
*    AND  mjahr EQ @w_zppt0002-budat+0(4)
*    AND  bwart EQ '261'.
*
*  IF sy-subrc NE 0.
*
*    DO 3 TIMES.
*      FREE: wa_return, is_block, p_erro.
*
*      vl_index = sy-index.
*
*      CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
*        EXPORTING
*          confirmation     = es_confirmation
*        IMPORTING
*          cancconfirmation = v_confirmation_es
*          return           = wa_return.
*
*      APPEND wa_return TO it_return.
*
*      IF wa_return-type = 'E'.
*        p_erro = abap_true.
*
*        CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
*          EXPORTING
*            id       = wa_return-id
*            number   = wa_return-number
*          IMPORTING
*            is_block = is_block.
*
*        l_erro_lock = is_block.
*
*        IF is_block IS NOT INITIAL AND vl_index NE 3.
*          WAIT UP TO 2 SECONDS.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ELSE.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    IF p_erro = abap_true.
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      l_mensagem = wa_return-message.
*      PERFORM f_gera_msg_carga_estorno USING '2'
*                                             ''
*                                             ''
*                                             l_mensagem.
*      EXIT.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = abap_true.
*
*    SELECT SINGLE *
*      INTO @DATA(gs_blpp_es)
*      FROM blpp
*     WHERE prtnr = @v_confirmation_es
*       AND prtps = '0001'.
*
*    WAIT UP TO 2 SECONDS.
*    DATA(doc_estorno) = gs_blpp_es-belnr.
*
*    wa_return-type    = 'S'.
*    CONCATENATE 'Documento Gerado/Estornado:' gs_blpp-belnr ' / ' doc_estorno INTO wa_return-message.
*    l_mensagem = wa_return-message.
*
*    PERFORM f_gera_msg_carga_estorno USING '3'
*                                           gs_blpp-belnr
*                                           sy-datum(4)
*                                           l_mensagem.
*  ENDIF.
*
*ENDFORM.

************************************************************************
* estorna documento fardinho
************************************************************************
FORM f_estorna_movto CHANGING p_erro.

  DATA: vl_index                 TYPE sy-index,
        wl_invoicedocnumber_migo TYPE bapi2017_gm_head_ret,
        is_block                 TYPE char01,
        l_lock                   TYPE c.

  FREE: p_erro,
        it_return.

  DO 3 TIMES.

    FREE: it_return, l_erro_lock, p_erro.

    vl_index = sy-index.

    CLEAR: wl_invoicedocnumber_migo.

    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument    = w_mseg-mblnr
        matdocumentyear     = w_mseg-mjahr
        goodsmvt_pstng_date = sy-datum
      IMPORTING
        goodsmvt_headret    = wl_invoicedocnumber_migo
      TABLES
        return              = it_return.

    SORT it_return BY type id number.
    DELETE ADJACENT DUPLICATES FROM it_return
                          COMPARING type id number.

    READ TABLE it_return INTO wa_return WITH KEY type = 'E'.

    IF sy-subrc IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      WAIT UP TO 5 SECONDS.

      DATA(l_index) = 0.

      IF wl_invoicedocnumber_migo-mat_doc IS NOT INITIAL.
        DO.
          l_index = sy-index.

          SELECT smbln
            INTO @DATA(l_smbln)
            FROM mseg
              UP TO 1 ROWS
           WHERE mblnr = @wl_invoicedocnumber_migo-mat_doc
             AND mjahr = @wl_invoicedocnumber_migo-doc_year.
          ENDSELECT.

          IF sy-subrc = 0.
            EXIT.
          ENDIF.

          IF l_index = 100.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.

      IF it_return[] IS INITIAL.
        wa_return-type    = 'S'.
        wa_return-message = 'Doc.Estorno=' && wl_invoicedocnumber_migo-mat_doc
                                           && '/'
                                           && wl_invoicedocnumber_migo-doc_year.
        l_mensagem = wa_return-message.

        PERFORM f_gera_msg_carga_estorno USING '3'
                                               wl_invoicedocnumber_migo-mat_doc
                                               wl_invoicedocnumber_migo-doc_year
                                               l_mensagem.
      ENDIF.
      EXIT.
    ELSE.
      p_erro = abap_true.

      CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
        EXPORTING
          id       = wa_return-id
          number   = wa_return-number
        IMPORTING
          is_block = is_block.

      l_erro_lock = abap_false.

      IF is_block IS NOT INITIAL.
        l_erro_lock = abap_true.
      ENDIF.

      IF ( is_block IS NOT INITIAL OR l_erro_lock = abap_true ) AND vl_index NE 3.
        WAIT UP TO 2 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

  IF p_erro = abap_true.
    l_mensagem = wa_return-message.
    PERFORM f_gera_msg_carga_estorno USING '2'
                                           ''
                                           ''
                                           l_mensagem.
  ENDIF.

ENDFORM.

************************************************************************
* estorna zmmt0008
************************************************************************
FORM f_estorna_zmmt0008.

  READ TABLE t_zmmt0008 INTO w_zmmt0008 WITH KEY werks       = w_zsdt0330-werks
                                                 lgort       = w_zsdt0330-lgort
                                                 charg       = w_zsdt0330-acharg
                                                 vbeln       = w_zsdt0330-vbeln
                                                 nr_romaneio = w_zsdt0330-nr_romaneio.
  CHECK sy-subrc = 0.

*----------------------------
* insere tabela historico
*----------------------------
  MOVE-CORRESPONDING w_zmmt0008 TO w_zmmt0008_delete.
  MOVE sy-uname                 TO w_zmmt0008_delete-user_delete.
  MOVE sy-datum                 TO w_zmmt0008_delete-data_delete.
  MOVE sy-uzeit                 TO w_zmmt0008_delete-hora_delete.
  MODIFY zmmt0008_delete      FROM w_zmmt0008_delete.

*----------------------------
* elimina registro atual
*----------------------------
  DELETE FROM zmmt0008 WHERE werks       = w_zsdt0330-werks
                         AND lgort       = w_zsdt0330-lgort
                         AND charg       = w_zsdt0330-acharg
                         AND vbeln       = w_zsdt0330-vbeln
                         AND nr_romaneio = w_zsdt0330-nr_romaneio.

  COMMIT WORK AND WAIT.

ENDFORM.

************************************************************************
* cancela linhas zsdt0330
************************************************************************
FORM f_cancela_zsdt0330.

 UPDATE zsdt0330 SET cancelado = abap_true
  WHERE id_carga  = w_zsdt0330-id_carga
    AND matnr     = w_zsdt0330-matnr
    AND werks     = w_zsdt0330-werks
    AND lgort     = w_zsdt0330-lgort
    AND acharg    = w_zsdt0330-acharg
    AND safra     = w_zsdt0330-safra
    AND seq       = w_zsdt0330-seq.

ENDFORM.

************************************************************************
*&-gerar erro para o fardo nos estornos
************************************************************************
FORM f_gera_msg_carga_estorno USING p_status
                                    p_mblnr
                                    p_mjahr
                                    p_mensagem.

  DATA: l_tipo_msg  TYPE bapi_mtype.

  l_tipo_msg = COND #( WHEN p_status = '3' THEN 'S'
                                           ELSE 'E' ).

  LOOP AT t_carga INTO w_carga.
    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_log( EXPORTING i_id_carga     = w_carga-id_carga
                                    i_matnr        = w_carga-matnr
                                    i_werks        = w_carga-werks
                                    i_lgort        = w_carga-lgort
                                    i_acharg       = w_carga-acharg
                                    i_safra        = w_carga-safra
                                    i_tipo_integra = 'ES'
                                    i_tipo_msg     = l_tipo_msg
                                    i_mensagem     = p_mensagem ).

    IF l_tipo_msg = 'S'.
      UPDATE zsdt0330 SET mblnr_estorno   = p_mblnr
                          mjahr_estorno   = p_mjahr
                          seq_estornado   = w_zsdt0330-seq
                          status_fardo    = p_status
                    WHERE id_carga      = w_carga-id_carga
                      AND matnr         = w_carga-matnr
                      AND werks         = w_carga-werks
                      AND lgort         = w_carga-lgort
                      AND acharg        = w_carga-acharg
                      AND safra         = w_carga-safra
                      AND seq           = w_carga-seq
                      AND cancelado     = abap_false.
    ELSE.
      UPDATE zsdt0330 SET status_fardo  = p_status
                    WHERE id_carga      = w_carga-id_carga
                      AND matnr         = w_carga-matnr
                      AND werks         = w_carga-werks
                      AND lgort         = w_carga-lgort
                      AND acharg        = w_carga-acharg
                      AND safra         = w_carga-safra
                      AND seq           = w_carga-seq
                      AND cancelado     = abap_false.
    ENDIF.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

************************************************************************
************************************************************************
.
