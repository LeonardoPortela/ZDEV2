*&---------------------------------------------------------------------*
*& Report  ZIM03
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zim03.

INCLUDE <icon>.


TABLES: csks, zim02_sol_ap_ctl.

TYPES: BEGIN OF ty_zim01_sol_ap_inv,
         bukrs              TYPE zim01_sol_ap_inv-bukrs,
         gsber              TYPE zim01_sol_ap_inv-gsber,
         ano                TYPE zim01_sol_ap_inv-ano,
         safra              TYPE zim01_sol_ap_inv-safra,
         safra2             TYPE zim01_sol_ap_inv-safra2,
         kostl              TYPE zim01_sol_ap_inv-kostl,
         buzei              TYPE zim01_sol_ap_inv-buzei,
         fase               TYPE zim01_sol_ap_inv-fase,
         izwek              TYPE zim01_sol_ap_inv-izwek,
         txt50              TYPE zim01_sol_ap_inv-txt50,
         objetivo           TYPE zim01_sol_ap_inv-objetivo,
         descr_item         TYPE zim01_sol_ap_inv-descr_item,
         menge              TYPE zim01_sol_ap_inv-menge,
         vlr_unitario       TYPE zim01_sol_ap_inv-vlr_unitario,
         vlr_total          TYPE zim01_sol_ap_inv-vlr_total,

         moeda              TYPE zim01_sol_ap_inv-moeda,
         dt_inicio          TYPE zim01_sol_ap_inv-dt_inicio,
         dt_fim             TYPE zim01_sol_ap_inv-dt_fim,
*leoobsi
         ano_fim_exec       TYPE zim01_sol_ap_inv-ano_fim_exec,
*leoobsf
         status_cta         TYPE zim01_sol_ap_inv-status_cta,
         knttp              TYPE zim01_sol_ap_inv-knttp,
         knttx              TYPE zim01_sol_ap_inv-knttx,
         observacoes        TYPE zim01_sol_ap_inv-observacoes,
         saknr              TYPE zim01_sol_ap_inv-saknr,
         txt20              TYPE zim01_sol_ap_inv-txt20,
         posnr              TYPE zim01_sol_ap_inv-posnr,
         solicitacao_invest TYPE zim01_sol_ap_inv-solicitacao_invest,
         status_aprov       TYPE zim01_sol_ap_inv-status_aprov,
         dt_aprovacao       TYPE zim01_sol_ap_inv-dt_aprovacao,
         aprovador          TYPE zim01_sol_ap_inv-aprovador,
         usuario            TYPE zim01_sol_ap_inv-usuario,
         data_entr          TYPE zim01_sol_ap_inv-data_entr,
         hora_entr          TYPE zim01_sol_ap_inv-hora_entr,
         data_mod           TYPE  dats, "ZIM01_SOL_AP_inv-DATA_MOD,
         hora_mod           TYPE zim01_sol_ap_inv-hora_mod,
         usuario_im         TYPE zim01_sol_ap_inv-usuario_im,
         data_entr_im       TYPE zim01_sol_ap_inv-data_entr_im,
         flag               TYPE zim01_sol_ap_inv-flag,
         tx_usd             TYPE zim01_sol_ap_inv-tx_usd,
         tx_eur             TYPE zim01_sol_ap_inv-tx_eur,
         vl_usd             TYPE zim01_sol_ap_inv-vl_usd,
         vl_eur             TYPE zim01_sol_ap_inv-vl_eur,
         finalidade         TYPE zim01_sol_ap_inv-finalidade,
         data_aprov         TYPE zim01_sol_ap_inv-data_aprov,
         hora_aprov         TYPE zim01_sol_ap_inv-hora_aprov,
         lote_anex          TYPE zim01_sol_ap_inv-lote_anex,
         lote_user          TYPE zim01_sol_ap_inv-lote_user,
         lote               TYPE zim01_sol_ap_inv-lote,
         lote3              TYPE zim01_sol_ap_inv-lote3,
         cod_gpo            TYPE zim01_sol_ap_inv-cod_gpo,
       END OF ty_zim01_sol_ap_inv,

       BEGIN OF ty_saida,
         icons(4),
         icona(4),
         bukrs              TYPE zim01_sol_ap_inv-bukrs,
         gsber              TYPE zim01_sol_ap_inv-gsber,
*Início Alteração Ricardo Furst.
         gtext              TYPE tgsbt-gtext,
*Fim Alteração Ricardo Furst.
         ano                TYPE zim01_sol_ap_inv-ano,
*         ano(10),
         safra(10),"         TYPE zim01_sol_ap_inv-safra,
*  safra2        TYPE zim01_sol_ap_inv-safra2,
         kostl              TYPE zim01_sol_ap_inv-kostl,
* Início Alteração Ricardo Furst.
         ltext              TYPE cskt-ltext,
* Fim Alteração Ricardo Furst.
         icon(45)           TYPE c,
         buzei              TYPE zim01_sol_ap_inv-buzei,
         fase               TYPE zim01_sol_ap_inv-fase,
         cod_gpo            TYPE zim01_sol_ap_inv-cod_gpo,
         name1              TYPE lfa1-name1,
*  MOTV(50)          TYPE c,
         maktx              TYPE makt-maktx,
         objetivo           TYPE zim01_sol_ap_inv-objetivo,
         descr_item         TYPE zim01_sol_ap_inv-descr_item,
         menge              TYPE zim01_sol_ap_inv-menge,
         vlr_unitario       TYPE zim01_sol_ap_inv-vlr_unitario,
         vlr_total          TYPE zim01_sol_ap_inv-vlr_total,
         moeda              TYPE zim01_sol_ap_inv-moeda,
         dt_inicio          TYPE zim01_sol_ap_inv-dt_inicio,
         dt_fim             TYPE zim01_sol_ap_inv-dt_fim,
*leoobsi
         ano_fim_exec       TYPE zim01_sol_ap_inv-ano_fim_exec,
*leoobsf
         status_cta         TYPE zim01_sol_ap_inv-status_cta,
*  CLDCT         TYPE STRING,
         sgtxt              TYPE bseg-sgtxt,
         observacoes        TYPE zim01_sol_ap_inv-observacoes,
*  NRTC(30),"          TYPE string,
         nrtc(10)           TYPE c,
* Início Alteração Ricardo Furst.
         nrtc_desc(20),
* Fim Alteração Ricardo Furst.
         posnr              TYPE zim01_sol_ap_inv-posnr,
         solicitacao_invest TYPE zim01_sol_ap_inv-solicitacao_invest,
*  status_aprov  TYPE zim01_sol_ap_inv-status_aprov,
         txtstat(20),
         dt_aprovacao       TYPE zim01_sol_ap_inv-dt_aprovacao,
         aprovador          TYPE zim01_sol_ap_inv-aprovador,
         nivel              TYPE zim12_aprov_inv-nivel,
         usuario            TYPE zim01_sol_ap_inv-usuario,
         data_entr          TYPE zim01_sol_ap_inv-data_entr,
         hora_entr          TYPE zim01_sol_ap_inv-hora_entr,
         data_mod           TYPE zim01_sol_ap_inv-data_mod,
         hora_mod           TYPE zim01_sol_ap_inv-hora_mod,
         usuario_im         TYPE zim01_sol_ap_inv-usuario_im,
         data_entr_im       TYPE zim01_sol_ap_inv-data_entr_im,
         flag               TYPE zim01_sol_ap_inv-flag,
         tx_usd             TYPE zim01_sol_ap_inv-tx_usd,
         tx_eur             TYPE zim01_sol_ap_inv-tx_eur,
         vl_usd             TYPE zim01_sol_ap_inv-vl_usd,
         vl_eur             TYPE zim01_sol_ap_inv-vl_eur,
         data_aprov         TYPE zim01_sol_ap_inv-data_aprov,
         hora_aprov         TYPE zim01_sol_ap_inv-hora_aprov,
         lote_anex          TYPE zim01_sol_ap_inv-lote_anex,
         lote_user          TYPE zim01_sol_ap_inv-lote_user,
         lote               TYPE zim01_sol_ap_inv-lote,
         lote3              TYPE zim01_sol_ap_inv-lote3,
         safra1             TYPE zim01_sol_ap_inv-safra,
         safra2             TYPE zim01_sol_ap_inv-safra2,
         nome(80),
         email(40),
*leoobsi
         responsavel        TYPE zim02_sol_ap_ctl-responsavel,

*leoobsf
         finalidade(30),
         atividade          TYPE zim02_sol_ap_ctl-atividade,
       END OF ty_saida,

       BEGIN OF ty_lote,
         lote TYPE zim01_sol_ap_inv-lote,
       END OF ty_lote,

* Início Alteração Ricardo Furst.
       BEGIN OF ty_cskt.
         INCLUDE STRUCTURE cskt.
TYPES:   END OF ty_cskt,

BEGIN OF ty_tgsbt.
  INCLUDE STRUCTURE tgsbt.
TYPES:   END OF ty_tgsbt.

TYPES: BEGIN OF ty_estra.
         INCLUDE STRUCTURE zfi_estrategia_zim.
TYPES:        area(20),
       END OF ty_estra.


*&---------------------------------------------------------------------*
*& Definições para ALV
*&---------------------------------------------------------------------*

TYPE-POOLS: slis,
            kkblo.


DATA: manager        TYPE REF TO cl_gos_manager.

DATA: repid           LIKE sy-repid.
DATA: fieldcat        TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: layout          TYPE slis_layout_alv.
DATA: print           TYPE slis_print_alv.
DATA: sort      TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      events    TYPE slis_t_event,
      xs_events TYPE slis_alv_event.

DATA: variante     LIKE disvariant,
      def_variante LIKE disvariant.
DATA: w_tit(70).

DATA: w_changed,
      w_leave,
      w_exit.

DATA: cl_alv TYPE REF TO cl_gui_alv_grid.
*&---------------------------------------------------------------------*
* TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: t_zim01_sol_ap_inv TYPE TABLE OF ty_zim01_sol_ap_inv,
      t_saida            TYPE TABLE OF ty_saida,
*leoosbi
      t_zim02_sol_ap_ctl LIKE zim02_sol_ap_ctl OCCURS 0 WITH HEADER LINE,
*leoobsf
* Início Alteração Ricardo Furst.
      t_cskt             TYPE TABLE OF ty_cskt,
      t_tgsbt            TYPE TABLE OF ty_tgsbt,
      tg_estra           TYPE TABLE OF ty_estra,
      tg_log             TYPE TABLE OF zim12_aprov_inv WITH HEADER LINE,
      it_lote            TYPE TABLE OF ty_lote WITH HEADER LINE,
      wg_estra           TYPE ty_estra.
* Fim Alteração Ricardo Furst.

*&---------------------------------------------------------------------*
*   WORK AREA
*&---------------------------------------------------------------------*

DATA: wa_zim01_sol_ap_inv TYPE ty_zim01_sol_ap_inv,
      wa_saida            TYPE ty_saida,
      wa_cskt             TYPE ty_cskt,
      wa_tgsbt            TYPE ty_tgsbt.


DATA: t_lotes TYPE TABLE OF  zfi_gru_inv,
      t_estra TYPE TABLE OF  zfi_estrategia_zim,
      w_estra TYPE zfi_estrategia_zim,
      t_docs  TYPE TABLE OF  zim01_sol_ap_inv,
      v_msg   TYPE char50.

DATA: ok-code                TYPE sy-ucomm,
      vg_cancel(1),
      gf_authorization_ft_09 TYPE c. "Workflow de Documentos

DATA: grid1               TYPE REF TO cl_gui_alv_grid,
      grid2               TYPE REF TO cl_gui_alv_grid,
      obg_conteiner_estra TYPE REF TO cl_gui_custom_container,
      g_cc_estra          TYPE scrfname VALUE 'CC_ESTRA',
      obg_conteiner_log   TYPE REF TO cl_gui_custom_container,
      g_cc_log            TYPE scrfname VALUE 'CC_LOG',
      wa_stable           TYPE lvc_s_stbl,
      t_fieldcatalog      TYPE lvc_t_fcat,
      w_fieldcatalog      TYPE lvc_s_fcat,
      wa_layout           TYPE lvc_s_layo.

*&---------------------------------------------------------------------*
* TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR wa_zim01_sol_ap_inv-bukrs OBLIGATORY,
                  s_gsber FOR wa_zim01_sol_ap_inv-gsber,
                  s_ano   FOR wa_zim01_sol_ap_inv-ano OBLIGATORY,
                  s_safra FOR wa_zim01_sol_ap_inv-safra,
                  s_kostl FOR csks-kostl,
                  so_fase FOR wa_zim01_sol_ap_inv-fase,
                  s_aprov FOR wa_zim01_sol_ap_inv-aprovador,
*leoobsi
                  s_resp  FOR zim02_sol_ap_ctl-responsavel.
*leoobsf
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  PARAMETERS: p_varia LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.

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
  ENDIF.

START-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Iniciar variaveis do ALV
*&---------------------------------------------------------------------*

  PERFORM alv_init.



  PERFORM seleciona_dados.

*&---------------------------------------------------------------------*
*&ORGANIZAR DADOS.
*&---------------------------------------------------------------------*

  DATA: w_per LIKE usr21-persnumber,
        w_add LIKE usr21-addrnumber.
*leoobsi
  SORT t_zim02_sol_ap_ctl BY bukrs ano safra kostl fase aprovador.
*leoobsf
  LOOP AT t_zim01_sol_ap_inv INTO wa_zim01_sol_ap_inv.
*leoobsi
    READ TABLE t_zim02_sol_ap_ctl WITH KEY bukrs  = wa_zim01_sol_ap_inv-bukrs
                                       ano       = wa_zim01_sol_ap_inv-ano
                                       safra     = wa_zim01_sol_ap_inv-safra
                                       kostl     = wa_zim01_sol_ap_inv-kostl
*                                       fase      = wa_zim01_sol_ap_inv-fase
*                                       APROVADOR = WA_ZIM01_SOL_AP_INV-APROVADOR
                                       BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE t_zim02_sol_ap_ctl-responsavel TO wa_saida-responsavel.
    ENDIF.

*leoobsf


    wa_saida-bukrs            = wa_zim01_sol_ap_inv-bukrs.
    wa_saida-gsber            = wa_zim01_sol_ap_inv-gsber.
*Início Alteração Ricardo Furst.
    READ TABLE t_tgsbt INTO wa_tgsbt WITH KEY gsber = wa_saida-gsber.
    IF sy-subrc EQ 0.
      wa_saida-gtext           = wa_tgsbt-gtext.
    ENDIF.
*Fim Alteração Ricardo Furst.
*   WA_SAIDA-SOL_APROV        = WA_ZIM01_SOL_AP_inv-SOL_APROV.

*    IF NOT wa_zim01_sol_ap_inv-safra IS INITIAL.
*      CONCATENATE wa_zim01_sol_ap_inv-ano 'Safra' wa_zim01_sol_ap_inv-safra '/' wa_zim01_sol_ap_inv-safra2
*      INTO wa_saida-ano SEPARATED BY space.
*    ELSE.
    wa_saida-ano              = wa_zim01_sol_ap_inv-ano.
    wa_saida-fase             = wa_zim01_sol_ap_inv-fase.
    wa_saida-cod_gpo          = wa_zim01_sol_ap_inv-cod_gpo.
*    ENDIF.

    IF NOT wa_zim01_sol_ap_inv-safra IS INITIAL.
      CONCATENATE wa_zim01_sol_ap_inv-safra '/' wa_zim01_sol_ap_inv-safra2
      INTO wa_saida-safra.
    ENDIF.
    wa_saida-safra1           = wa_zim01_sol_ap_inv-safra.
    wa_saida-safra2           = wa_zim01_sol_ap_inv-safra2.
    wa_saida-kostl            = wa_zim01_sol_ap_inv-kostl.

* Início Alteração Ricardo Furst.
    SORT t_cskt BY kostl.
    READ TABLE t_cskt INTO wa_cskt WITH KEY kostl = wa_zim01_sol_ap_inv-kostl.
    IF sy-subrc EQ 0.
      wa_saida-ltext         = wa_cskt-ltext.
    ENDIF.
* Fim alteração Ricardo Furst.

    wa_saida-buzei            = wa_zim01_sol_ap_inv-buzei.
*    wa_saida-fase             = wa_zim01_sol_ap_inv-fase.
    IF wa_zim01_sol_ap_inv-fase = 1.
      wa_saida-name1 = 'Planejado'.
    ELSEIF wa_zim01_sol_ap_inv-fase = 2.
      wa_saida-name1 = 'Extra'.
    ELSE.
      wa_saida-name1 = 'Complemento'.
    ENDIF.
    CONCATENATE wa_zim01_sol_ap_inv-izwek wa_zim01_sol_ap_inv-txt50 INTO wa_saida-maktx
                 SEPARATED BY ' - '.
    wa_saida-objetivo         = wa_zim01_sol_ap_inv-objetivo.
    wa_saida-descr_item       = wa_zim01_sol_ap_inv-descr_item.
    wa_saida-menge            = wa_zim01_sol_ap_inv-menge.
    wa_saida-vlr_unitario     = wa_zim01_sol_ap_inv-vlr_unitario.
    wa_saida-vlr_total        = wa_zim01_sol_ap_inv-vlr_total.
    wa_saida-moeda            = wa_zim01_sol_ap_inv-moeda.
    wa_saida-dt_inicio        = wa_zim01_sol_ap_inv-dt_inicio.
    wa_saida-dt_fim           = wa_zim01_sol_ap_inv-dt_fim.
*leoobsi
    IF wa_zim01_sol_ap_inv-ano_fim_exec IS INITIAL.
      wa_saida-ano_fim_exec     = wa_zim01_sol_ap_inv-dt_fim(4).
    ELSE.
      wa_saida-ano_fim_exec     = wa_zim01_sol_ap_inv-ano_fim_exec.
    ENDIF.
*leoobsf
    wa_saida-status_cta       = wa_zim01_sol_ap_inv-status_cta.


    CONCATENATE wa_zim01_sol_ap_inv-knttp wa_zim01_sol_ap_inv-knttx INTO wa_saida-sgtxt
                 SEPARATED BY ' - '.
    wa_saida-observacoes      = wa_zim01_sol_ap_inv-observacoes.
* Início Alteração Ricardo Furst.
    wa_saida-nrtc_desc        = wa_zim01_sol_ap_inv-txt20.
    wa_saida-nrtc             = wa_zim01_sol_ap_inv-saknr+4.
*    CONCATENATE WA_ZIM01_SOL_AP_INV-SAKNR+4 WA_ZIM01_SOL_AP_INV-TXT20 INTO WA_SAIDA-NRTC
*                 SEPARATED BY ' - '.
* Fim Alteração Ricardo Furst.
*    wa_saida-nrtc = wa_zim01_sol_ap_inv-saknr+4.


    wa_saida-posnr              = wa_zim01_sol_ap_inv-posnr.
    wa_saida-solicitacao_invest = wa_zim01_sol_ap_inv-solicitacao_invest.
*    wa_saida-status_aprov     = wa_zim01_sol_ap_inv-status_aprov.

    IF wa_zim01_sol_ap_inv-status_aprov EQ '1'.
      wa_saida-txtstat = 'Aprovado'.
    ELSEIF wa_zim01_sol_ap_inv-status_aprov EQ '2'.
      wa_saida-txtstat = 'Reprovado'.
    ELSEIF wa_zim01_sol_ap_inv-status_aprov EQ '3'.
      wa_saida-txtstat = 'Bloqueado'.
    ELSE.
      CLEAR wa_saida-txtstat.
    ENDIF.
    "
    wa_saida-lote             = wa_zim01_sol_ap_inv-lote.
    wa_saida-lote3            = wa_zim01_sol_ap_inv-lote3.
    CLEAR: wa_saida-dt_aprovacao, wa_saida-aprovador.

    IF wa_zim01_sol_ap_inv-lote IS NOT INITIAL.
      REFRESH tg_log.
      SELECT *
        FROM zim12_aprov_inv
        APPENDING CORRESPONDING FIELDS OF TABLE tg_log
        WHERE kostl = wa_saida-kostl
        AND   lote  = wa_saida-lote.

      IF tg_log[] IS NOT INITIAL.
*        SELECT *
*         FROM ZIM12_APROV
*         INTO TABLE @DATA(IT_ZIM12_APROV)
*         WHERE   KOSTL = @WA_SAIDA-KOSTL.
*        LOOP AT IT_ZIM12_APROV INTO DATA(W_ZIM12).
*          READ TABLE TG_LOG WITH KEY KOSTL = W_ZIM12-KOSTL
*                                     NIVEL = W_ZIM12-NIVEL.
*          IF SY-SUBRC NE 0.
*            READ TABLE TG_LOG INTO DATA(WLOG)
*                    WITH KEY APROVADOR = W_ZIM12-APROVADOR.
*            IF SY-SUBRC = 0.
*              MOVE-CORRESPONDING WLOG TO TG_LOG.
*              WLOG-NIVEL = W_ZIM12-NIVEL.
*              WLOG-FASE = W_ZIM12-FASE.
*              APPEND WLOG TO TG_LOG.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
        SORT tg_log BY nivel DESCENDING.
        READ TABLE tg_log INDEX 1.
        wa_saida-dt_aprovacao     = tg_log-data_aprov.
        wa_saida-aprovador        = tg_log-aprovador.
        wa_saida-nivel            = tg_log-nivel.
        REFRESH tg_log.
      ENDIF.

    ENDIF.

    IF wa_saida-aprovador IS INITIAL.
      READ TABLE t_zim02_sol_ap_ctl WITH KEY bukrs  = wa_zim01_sol_ap_inv-bukrs
                                         ano       = wa_zim01_sol_ap_inv-ano
                                         safra     = wa_zim01_sol_ap_inv-safra
                                         kostl     = wa_zim01_sol_ap_inv-kostl
                                         fase      = wa_zim01_sol_ap_inv-fase
*                                         APROVADOR = WA_ZIM01_SOL_AP_INV-APROVADOR
                                         BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE t_zim02_sol_ap_ctl-aprovador TO wa_saida-aprovador.
      ENDIF.
    ENDIF.
    wa_saida-usuario          = wa_zim01_sol_ap_inv-usuario.
    wa_saida-data_entr        = wa_zim01_sol_ap_inv-data_entr.
    wa_saida-hora_entr        = wa_zim01_sol_ap_inv-hora_entr.
    wa_saida-data_mod         = wa_zim01_sol_ap_inv-data_mod.
    wa_saida-hora_mod         = wa_zim01_sol_ap_inv-hora_mod.
    wa_saida-usuario_im       = wa_zim01_sol_ap_inv-usuario_im.
    wa_saida-data_entr_im     = wa_zim01_sol_ap_inv-data_entr_im.
    wa_saida-flag             = wa_zim01_sol_ap_inv-flag.
    wa_saida-tx_usd           = wa_zim01_sol_ap_inv-tx_usd.
    wa_saida-tx_eur           = wa_zim01_sol_ap_inv-tx_eur.
    wa_saida-vl_usd           = wa_zim01_sol_ap_inv-vl_usd.
*    wa_saida-vl_usd           = wa_zim01_sol_ap_inv-vlr_total
*                              / wa_zim01_sol_ap_inv-tx_usd.
    wa_saida-vl_eur           = wa_zim01_sol_ap_inv-vl_eur.
    wa_saida-data_aprov       = wa_zim01_sol_ap_inv-data_aprov.
    wa_saida-hora_aprov       = wa_zim01_sol_ap_inv-hora_aprov.
    wa_saida-lote_anex        = wa_zim01_sol_ap_inv-lote_anex.
    wa_saida-lote_user        = wa_zim01_sol_ap_inv-lote_user.
    wa_saida-lote             = wa_zim01_sol_ap_inv-lote.
    wa_saida-lote3            = wa_zim01_sol_ap_inv-lote3.
    IF wa_saida-lote_anex IS NOT INITIAL.
      wa_saida-icona = icon_gos_services_relations.
    ENDIF.
    IF wa_saida-lote_user IS NOT INITIAL.
      wa_saida-icons = icon_gos_services_relations.
    ENDIF.

    IF wa_saida-txtstat IS INITIAL.
      CASE wa_zim01_sol_ap_inv-status_cta.
        WHEN 1.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              name                  = icon_yellow_light
              info                  = 'Análise Contabilidade'
              add_stdinf            = 'X'
            IMPORTING
              result                = wa_saida-icon
            EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2
              OTHERS                = 3.

*        wa_saida-icon = icon_yellow_light.
        WHEN 2.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              name                  = icon_green_light
              info                  = 'Aprovado Contabilidade'
              add_stdinf            = 'X'
            IMPORTING
              result                = wa_saida-icon
            EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2
              OTHERS                = 3.

*        wa_saida-icon = icon_green_light.
        WHEN 3.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              name                  = icon_red_light
              info                  = 'Reprovado Contabilidade'
              add_stdinf            = 'X'
            IMPORTING
              result                = wa_saida-icon
            EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2
              OTHERS                = 3.

*        wa_saida-icon = icon_red_light.
        WHEN OTHERS.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              name                  = icon_light_out
              info                  = 'Ação do Usuário'
              add_stdinf            = 'X'
            IMPORTING
              result                = wa_saida-icon
            EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2
              OTHERS                = 3.

*        wa_saida-icon = icon_message_question_small.
      ENDCASE.

    ELSE.
      CASE wa_saida-txtstat.
        WHEN 'Aprovado'.

          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              name                  = icon_system_okay
              info                  = 'Aprovado Diretoria'
              add_stdinf            = 'X'
            IMPORTING
              result                = wa_saida-icon
            EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2
              OTHERS                = 3.
        WHEN 'Reprovado'.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              name                  = icon_message_critical
              info                  = 'Reprovado Diretoria'
              add_stdinf            = 'X'
            IMPORTING
              result                = wa_saida-icon
            EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2
              OTHERS                = 3.
        WHEN 'Bloqueado'.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              name                  = icon_locked
              info                  = 'Bloqueado Diretoria'
              add_stdinf            = 'X'
            IMPORTING
              result                = wa_saida-icon
            EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2
              OTHERS                = 3.
      ENDCASE.
    ENDIF.



    SELECT SINGLE persnumber addrnumber FROM usr21
      INTO (w_per, w_add)
      WHERE bname = wa_saida-usuario.
    IF sy-subrc = 0.
      SELECT SINGLE name_text FROM adrp
        INTO wa_saida-nome
        WHERE persnumber = w_per.

      SELECT SINGLE smtp_addr FROM adr6
        INTO wa_saida-email
        WHERE addrnumber = w_add AND
              persnumber = w_per  .

    ENDIF.

    CASE wa_zim01_sol_ap_inv-finalidade.
      WHEN '01'.
        CONCATENATE '01' 'Meio ambiente'   INTO wa_saida-finalidade SEPARATED BY ' - '.
      WHEN '02'.
        CONCATENATE '02' 'Obrigação Legal' INTO wa_saida-finalidade SEPARATED BY ' - '.
      WHEN '03'.
        CONCATENATE '03' 'Sinistro'        INTO wa_saida-finalidade SEPARATED BY ' - '.
      WHEN '04'.
        CONCATENATE '04' 'Reserva'         INTO wa_saida-finalidade SEPARATED BY ' - '.
      WHEN '05'.
        CONCATENATE '05' 'Outros'          INTO wa_saida-finalidade SEPARATED BY ' - '.
      WHEN OTHERS.
        wa_saida-finalidade = wa_zim01_sol_ap_inv-finalidade.
    ENDCASE.


    wa_saida-atividade = t_zim02_sol_ap_ctl-atividade.

    "ALRS
*    IF WA_SAIDA-APROVADOR IS INITIAL.
*      READ TABLE T_ZIM02_SOL_AP_CTL WITH KEY BUKRS = WA_SAIDA-BUKRS
*                                             ANO   = WA_SAIDA-ANO
*                                             KOSTL = WA_SAIDA-KOSTL.
*      IF SY-SUBRC = 0.
*        WA_SAIDA-APROVADOR = T_ZIM02_SOL_AP_CTL-APROVADOR.
*      ENDIF.
*    ENDIF.

    APPEND wa_saida TO t_saida.

    CLEAR: wa_saida,
           wa_zim01_sol_ap_inv.

  ENDLOOP.

  PERFORM chama_alv.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*

FORM seleciona_dados .

*****CS2021000770 Eliminar transação ZIM03
*  SELECT *
*  FROM zim02_sol_ap_ctl
*    INTO TABLE t_zim02_sol_ap_ctl
*    WHERE bukrs     IN s_bukrs
*    AND ano         IN s_ano
*    AND safra       IN s_safra
**SAFRA2?
*    AND kostl       IN s_kostl
*    AND aprovador   IN s_aprov
**    AND FASE        IN SO_FASE
*    AND responsavel IN s_resp.
*  IF sy-subrc <> 0.
*
*    MESSAGE e398(00) WITH 'Nenhum dado encontrado de acordo'
*                          'com os parâmetros informados.'.
*    STOP.
*  ENDIF.

*  SELECT bukrs  gsber ano   safra     safra2
*         kostl  buzei fase  izwek     txt50
*         objetivo  descr_item  menge     vlr_unitario
*         vlr_total  moeda       dt_inicio dt_fim ano_fim_exec
*         status_cta knttp       knttx     observacoes
*         saknr   txt20       posnr     solicitacao_invest status_aprov
*         dt_aprovacao aprovador   usuario   data_entr
*         hora_entr    data_mod    hora_mod  usuario_im
*         data_entr_im flag        tx_usd    tx_eur
*         vl_usd       vl_eur      finalidade  data_aprov hora_aprov
*         lote_anex lote_user lote lote3 cod_gpo
*
*  FROM zim01_sol_ap_inv INTO TABLE t_zim01_sol_ap_inv
*  FOR ALL ENTRIES IN t_zim02_sol_ap_ctl
*
*    WHERE bukrs   = t_zim02_sol_ap_ctl-bukrs "IN S_BUKRS
*    AND gsber     IN s_gsber
*    AND ano       = t_zim02_sol_ap_ctl-ano "IN S_ANO
*    AND safra     = t_zim02_sol_ap_ctl-safra "IN S_SAFRA
*    AND kostl     = t_zim02_sol_ap_ctl-kostl "IN S_KOSTL
**    AND FASE      = T_ZIM02_SOL_AP_CTL-FASE "IN SO_FASE
*    AND fase      IN so_fase
*    AND aprovador IN s_aprov. "= T_ZIM02_SOL_AP_CTL-APROVADOR.

  SELECT bukrs  gsber ano   safra     safra2
         kostl  buzei fase  izwek     txt50
         objetivo  descr_item  menge     vlr_unitario
         vlr_total  moeda       dt_inicio dt_fim ano_fim_exec
         status_cta knttp       knttx     observacoes
         saknr   txt20       posnr     solicitacao_invest status_aprov
         dt_aprovacao aprovador   usuario   data_entr
         hora_entr    data_mod    hora_mod  usuario_im
         data_entr_im flag        tx_usd    tx_eur
         vl_usd       vl_eur      finalidade  data_aprov hora_aprov
         lote_anex lote_user lote lote3 cod_gpo

  FROM zim01_sol_ap_inv INTO TABLE t_zim01_sol_ap_inv
    WHERE bukrs   IN s_bukrs
    AND gsber     IN s_gsber
    AND ano       IN s_ano
    AND safra     IN s_safra
    AND kostl     IN s_kostl
*    AND FASE     IN SO_FASE
    AND fase      IN so_fase
    AND aprovador IN s_aprov.
***CS2021000770 Eliminar transação ZIM03

* Início Alteração Ricardo Furst.
  IF sy-subrc EQ 0.

    SELECT *
      FROM cskt
      INTO TABLE t_cskt
      FOR ALL ENTRIES IN t_zim01_sol_ap_inv
      WHERE kostl EQ t_zim01_sol_ap_inv-kostl AND
            datbi GE sy-datum.

    IF sy-subrc EQ 0.

      SORT t_cskt.

    ENDIF.

    SELECT *
      FROM tgsbt
      INTO TABLE t_tgsbt
      FOR ALL ENTRIES IN t_zim01_sol_ap_inv
      WHERE gsber EQ t_zim01_sol_ap_inv-gsber.

    IF sy-subrc EQ 0.

      SORT t_tgsbt.

    ENDIF.

  ENDIF.
* Fim Alteração Ricardo Furst.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ALV_INIT
*&---------------------------------------------------------------------*

FORM alv_init .

  CLEAR: variante.
  repid = sy-repid.

  variante-report = repid.

  IF p_varia IS INITIAL.

    CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = variante
      EXCEPTIONS
        not_found  = 2.
    IF sy-subrc = 0.
      p_varia = variante-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " ALV_INIT
*&---------------------------------------------------------------------*
*&      Form  CHAMA_ALV
*&---------------------------------------------------------------------*

FORM chama_alv .
*  FIELDCAT-JUST          = X_JUST  (X)
  PERFORM definir_eventos.
  REFRESH: fieldcat.


  PERFORM monta_fieldcat USING:
      'ICONS'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Anex.Sol                       ' ' ' ' ' 'X' ' ' ' ',
      'ICONA'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Anex.Apr                       ' ' ' ' ' 'X' ' ' ' ',
      'BUKRS'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Empresa                        ' ' ' ' ' 'X' ' ' ' ',
      'GSBER'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Filial                         ' ' ' ' ' 'X' ' ' ' ',
* Início Alteração Ricardo Furst.
      'GTEXT'          'T_SAIDA' 'TGSBT'            'Descrição Filial               ' ' ' ' ' 'X' ' ' ' ',
* Fim Alteração Ricardo Furst.
      'ANO'            'T_SAIDA' 'ZIM01_SOL_AP_INV ' '                               ' ' ' ' ' ' ' ' ' ' ',
      'SAFRA'          'T_SAIDA' '                 ' 'Safra                          ' ' ' ' ' ' ' ' ' ' ',
*      'SAFRA2'         'T_SAIDA' 'ZIM01_SOL_AP_INV' '                               ' ' ' ' ' ' ' ' ',
      'KOSTL'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Centro Custo                   ' ' ' ' ' 'X' 'X' ' ',
* Início Alteração Ricardo Furst.
      'LTEXT'          'T_SAIDA' 'CSKT'             'Descrição Centro Custo         ' ' ' ' ' 'X' ' ' ' ',
* Fim Alteração Ricardo Furst.
      'ICON'           'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Status                         ' ' ' ' ' 'X' ' ' ' ',
      'BUZEI'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Seq.                           ' ' ' ' ' ' ' ' ' ' ',
      'NAME1'           'T_SAIDA' 'LFA1' 'Fase'                                     ' ' ' ' ' ' ' ' '10',
      'ATIVIDADE'       'T_SAIDA' 'ATIVIDADE'  'Atividade'                          ' ' ' ' ' ' ' ' '15',
      'FINALIDADE'      'T_SAIDA' 'FINALIDADE' 'Finalidade'                         ' ' ' ' ' ' ' ' '25',


*      'MOTV'           'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Categoria do investimento      ' ' ' ' ' ' ' ' ' '25',
      'MAKTX'           'T_SAIDA' 'MAKT' 'Categoria do investimento      ' ' ' ' ' ' ' ' ' '25',
      'OBJETIVO'       'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Objetivo do investimento       ' ' ' ' ' ' ' ' ' ' ',
      'DESCR_ITEM'     'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Descrição do item              ' ' ' ' ' ' ' ' ' ' ',
      'MENGE'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Quant                          ' ' ' ' ' ' ' ' ' ' ',
      'VLR_UNITARIO'   'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Unitário R$                    ' ' ' ' ' ' ' ' ' ' ',
      'VLR_TOTAL'      'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Vl. Total R$                   ' ' ' ' ' ' ' ' ' ' ',
      'VL_USD'         'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Vl. Total US$                  ' ' ' ' ' ' ' ' ' ' ',
      'TX_USD'         'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Tx US$                         ' ' ' ' ' ' ' ' ' ' ',
      'VL_EUR'         'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Vl. Total EUR                  ' ' ' ' ' ' ' ' ' ' ',
      'TX_EUR'         'T_SAIDA' 'ZIM01_SOL_AP_INV' 'TX EUR                         ' ' ' ' ' ' ' ' ' ' ',
*      'MOEDA'          'T_SAIDA' 'ZIM01_SOL_AP_INV' '                              ' ' ' ' ' ' ' ' ' ' ',
      'DT_INICIO'      'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Dt.Inicio                      ' ' ' ' ' ' ' ' ' ' ',
      'DT_FIM'         'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Dt.fim                         ' ' ' ' ' ' ' ' ' ' ',
*leoobsi
'ANO_FIM_EXEC'         'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Ano fim                        ' ' ' ' ' ' ' ' ' ' ',
*leoobsf

*      'STATUS_CTA'     'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Status Contábil                ' ' ' ' ' ' ' ' ',
*      'CLDCT'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Classificação-Contábil         ' ' ' ' ' ' ' ' ' ' ',
      'SGTXT'          'T_SAIDA' 'BSEG'  'Classificação-Contábil         ' ' ' ' ' ' ' ' ' ' ',
      'OBSERVACOES'    'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Observação-Contábil            ' ' ' ' ' ' ' ' ' ' ',
      'NRTC'           'T_SAIDA' ' '                'Nr.Conta-Contábil              ' ' ' ' ' ' ' ' ' '10',
      'NRTC_DESC'      'T_SAIDA' ' '                'Descrição da C.Contábil        ' ' ' ' ' ' ' ' ' '20',
      'POSNR'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Solicitação de Investimento    ' ' ' ' ' ' ' ' ' ' ',
'SOLICITACAO_INVEST'   'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Investimento Sysphera          ' ' ' ' ' ' ' ' ' ' ',
*      'STATUS_APROV'   'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Status Aprovação               ' ' ' ' ' ' ' ' ',txtstat
      'TXTSTAT'        'T_SAIDA' '                ' 'Status Aprovação               ' ' ' ' ' ' ' ' ' ' ',
      'LOTE'           'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Lote Aprov.                    ' ' ' ' ' ' ' ' ' ' ',
      'DT_APROVACAO'   'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Dt.Ult.Aprov.                  ' ' ' ' ' ' ' ' ' ' ',
      'APROVADOR'      'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Aprovador                      ' ' ' ' ' ' ' ' ' ' ',
      'NIVEL'          'T_SAIDA' 'ZIM12_APROV_INV'  'Nivel                          ' ' ' ' ' ' ' ' ' ' ',
      'USUARIO'        'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Usuario                        ' ' ' ' ' ' ' ' ' ' ',
      'NOME'           'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Nome                           ' ' ' ' ' ' ' ' ' ' ',
      'EMAIL'          'T_SAIDA' 'ZIM01_SOL_AP_INV' 'E-Mail                         ' ' ' ' ' ' ' ' ' ' ',
      'DATA_ENTR'      'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Dt.Entrada                     ' ' ' ' ' ' ' ' ' ' ',
      'HORA_ENTR'      'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Hora Entrada                   ' ' ' ' ' ' ' ' ' ' ',
      'DATA_MOD'       'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Dt.Modificação                 ' ' ' ' ' ' ' ' ' ' ',
      'HORA_MOD'       'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Hora Modificação               ' ' ' ' ' ' ' ' ' ' ',
      'USUARIO_IM'     'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Usuário Contábil               ' ' ' ' ' ' ' ' ' ' ',
      'DATA_ENTR_IM'   'T_SAIDA' 'ZIM01_SOL_AP_INV' 'Dt.Contábil                    ' ' ' ' ' ' ' ' ' ' ',
      'RESPONSAVEL'    'T_SAIDA' 'ZIM02_SOL_AP_CTL' 'Resp. C.Custo                  ' ' ' ' ' ' ' ' ' ' '.

*      'FLAG'           'T_SAIDA' 'ZIM01_SOL_AP_INV' '                              ' ' ' ' ' ' ' ' ',

  w_tit = 'Relatório Solicitação de aprovação de investimento'.

  layout-zebra                 = 'X'.
  print-no_print_listinfos      = 'X'.
  variante-variant              = p_varia.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'COMANDO'
      it_fieldcat              = fieldcat[]
      it_sort                  = sort[]
      is_layout                = layout
      i_grid_title             = w_tit
      i_default                = 'X'
      i_save                   = 'A'
      it_events                = events
      is_variant               = variante
      is_print                 = print
    TABLES
      t_outtab                 = t_saida
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " CHAMA_ALV

FORM set_pf_status USING rt_extab TYPE slis_t_extab.        "#EC CALLED
  DESCRIBE TABLE rt_extab. "Avoid Extended Check Warning
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "Set_pf_status

*----------------------------------------------------------------------*
FORM monta_fieldcat USING
               x_field x_tab x_ref x_text x_sum x_just x_key x_hotspot x_outputlen .
*----------------------------------------------------------------------*
*
  DATA: wl_tam TYPE i.

  wl_tam = strlen( x_text ).
  wl_tam = wl_tam + 1.
  fieldcat-fieldname     = x_field.
  fieldcat-tabname       = x_tab.
  fieldcat-ref_tabname   = x_ref.
  fieldcat-do_sum        = x_sum.
  fieldcat-just          = x_just.
  fieldcat-key           = x_key.
  fieldcat-hotspot       = x_hotspot.
  fieldcat-seltext_l     =
  fieldcat-seltext_m     =
  fieldcat-seltext_s     =
  fieldcat-reptext_ddic  = x_text.
  IF wl_tam > x_outputlen.
    fieldcat-outputlen     =   wl_tam.
  ELSE.
* Início Alteração Ricardo Furst.
    fieldcat-outputlen     = x_outputlen.
  ENDIF.


* Fim Alteração Ricardo Furst.
  IF x_field = 'ICON' OR x_field = 'ICONA' OR x_field = 'ICONS'.
    fieldcat-outputlen = '4'.
    fieldcat-icon = 'X'.
  ENDIF.
  IF x_field = 'NRTC_DESC'.
    fieldcat-lowercase = 'X'.
  ENDIF.

  APPEND fieldcat.
  CLEAR fieldcat.
*
ENDFORM.                               " MONTA_FIELDCAT


*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
FORM f_pf_status USING t_excl TYPE slis_t_extab.

  SET PF-STATUS 'ZALV' EXCLUDING t_excl.

ENDFORM.                    "SET_STATUS


*---------------------------------------------------------------------*
*       FORM COMANDO                                            *
*---------------------------------------------------------------------*
*       ........"evento para HotSpot                                                      *
*---------------------------------------------------------------------*
FORM comando USING ucomm LIKE sy-ucomm
                         selfield TYPE kkblo_selfield.
  selfield = selfield.                                      "#EC CALLED

  DATA: w_val    TYPE setleaf-valfrom,
        vseq(10) TYPE p.

  DATA: auth_field1 LIKE tobj-fiel1,
        auth_field2 LIKE tobj-fiel1,
        auth_field3 LIKE tobj-fiel1,
        auth_value1 LIKE xu180-value,
        auth_value2 LIKE xu180-value,
        auth_value3 LIKE xu180-value,
        auth_user   LIKE usr04-bname,
        auth_object LIKE usr12-objct.

  CLEAR:auth_field1,
        auth_value1,
        auth_field2,
        auth_value2,
        auth_field3,
        auth_value3.

  DATA: wa_obj  TYPE borident,
        ip_mode TYPE sgs_rwmod.

  CASE ucomm.
    WHEN 'ANEXAR'.
      READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.
      IF  wa_saida-lote_user IS INITIAL.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZID_LOTEIV'
          IMPORTING
            number      = vseq.
        wa_saida-lote_user = vseq.
        MODIFY t_saida FROM wa_saida INDEX selfield-tabindex TRANSPORTING lote_user.
        UPDATE zim01_sol_ap_inv SET lote_user = wa_saida-lote_user
        WHERE bukrs  = wa_saida-bukrs
        AND   gsber  = wa_saida-gsber
        AND   ano    = wa_saida-ano
        AND   safra  = wa_saida-safra1
        AND   safra2 = wa_saida-safra2
        AND   kostl  = wa_saida-kostl
        AND   buzei  = wa_saida-buzei.
        COMMIT WORK.
      ENDIF.
      REFRESH: it_lote.
      gf_authorization_ft_09 = abap_false.
      CLEAR: it_lote.
      "Somente validar acesso para modificar
      it_lote = wa_saida-lote_user.
      APPEND it_lote.

      "workflow documentos
      IF manager IS NOT INITIAL.
        CALL METHOD manager->unpublish.
        CLEAR: manager.
      ENDIF.
      IF it_lote[] IS NOT INITIAL.
        READ TABLE it_lote INDEX 1.
        wa_obj-objtype = 'ZIM06'.
        CONCATENATE sy-mandt it_lote-lote INTO wa_obj-objkey.

        IF gf_authorization_ft_09 EQ abap_false.
          ip_mode = 'E'.
        ELSE.
          ip_mode = 'D'.
        ENDIF.

        CREATE OBJECT manager
          EXPORTING
            is_object        = wa_obj
            ip_no_commit     = 'R'
            ip_mode          = ip_mode
          EXCEPTIONS
            object_invalid   = 1
            callback_invalid = 2
            OTHERS           = 3.

        SET TITLEBAR 'TLWORK' WITH it_lote-lote.

      ELSE.

      ENDIF.

    WHEN 'VERANEX'.
      READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.
      IF  wa_saida-lote_anex IS INITIAL.
        MESSAGE 'Não há anexos' TYPE 'I'.
      ENDIF.
      REFRESH: it_lote.
      gf_authorization_ft_09 = abap_true.
      CLEAR: it_lote.
      "Somente validar acesso para modificar
      it_lote = wa_saida-lote_anex.
      APPEND it_lote.



      "workflow documentos
      IF manager IS NOT INITIAL.
        CALL METHOD manager->unpublish.
        CLEAR: manager.
      ENDIF.
      IF it_lote[] IS NOT INITIAL.
        READ TABLE it_lote INDEX 1.
        wa_obj-objtype = 'ZIM06'.
        CONCATENATE sy-mandt it_lote-lote INTO wa_obj-objkey.

        IF gf_authorization_ft_09 EQ abap_false.
          ip_mode = 'E'.
        ELSE.
          ip_mode = 'D'.
        ENDIF.

        CREATE OBJECT manager
          EXPORTING
            is_object        = wa_obj
            ip_no_commit     = 'R'
            ip_mode          = ip_mode
          EXCEPTIONS
            object_invalid   = 1
            callback_invalid = 2
            OTHERS           = 3.

        SET TITLEBAR 'TLWORK' WITH it_lote-lote.

      ELSE.

      ENDIF.


    WHEN '&EST'.
      REFRESH: t_lotes,t_estra,t_docs, tg_estra.
      READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.

      CALL FUNCTION 'Z_IM_ESTRATEGIA_LISTA'
        EXPORTING
          v_usuario = sy-uname
          v_kostl   = wa_saida-kostl
          v_lote    = wa_saida-lote
          v_cod_gpo = wa_saida-cod_gpo
          v_fase    = wa_saida-fase
          v_ano     = wa_saida-ano
          v_buzei   = wa_saida-buzei
        IMPORTING
          msg       = v_msg
        TABLES
          t_lotes   = t_lotes
          t_estra   = t_estra
          t_docs    = t_docs.

      LOOP AT t_estra INTO w_estra.
        MOVE-CORRESPONDING w_estra TO wg_estra.
        CLEAR wg_estra-area.
        IF wg_estra-gpo_cmp = 3.
          wg_estra-area = 'Informática'.
        ENDIF.
        APPEND wg_estra TO tg_estra.
      ENDLOOP.

      REFRESH: t_lotes,t_estra,t_docs.
      IF wa_saida-lote3 IS NOT INITIAL.
        CALL FUNCTION 'Z_IM_ESTRATEGIA_LISTA'
          EXPORTING
            v_usuario = sy-uname
            v_kostl   = wa_saida-kostl
            v_lote    = wa_saida-lote3
            v_cod_gpo = wa_saida-cod_gpo
            v_fase    = wa_saida-fase
            v_ano     = wa_saida-ano
            v_buzei   = wa_saida-buzei
          IMPORTING
            msg       = v_msg
          TABLES
            t_lotes   = t_lotes
            t_estra   = t_estra
            t_docs    = t_docs.

        LOOP AT t_estra INTO w_estra.
          MOVE-CORRESPONDING w_estra TO wg_estra.
          CLEAR wg_estra-area.
          IF wg_estra-gpo_cmp = 3.
            wg_estra-area = 'Informática'.
          ENDIF.
          APPEND wg_estra TO tg_estra.
        ENDLOOP.
      ENDIF.

      SORT tg_estra BY lote ano bukrs werks safra safra2 kostl buzei fase aprovador nivel.
      DELETE ADJACENT DUPLICATES FROM tg_estra COMPARING ALL FIELDS.
      SORT tg_estra BY gpo_cmp DESCENDING nivel ASCENDING.

      CALL SCREEN 0100 STARTING AT 030 3
                       ENDING   AT 150 10.
    WHEN '&LOG'.
      REFRESH tg_log.
      READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.
      SELECT *
       FROM zim12_aval_inv
       INTO CORRESPONDING FIELDS OF TABLE tg_log
       WHERE  lote  = wa_saida-lote3.

      SELECT *
        FROM zim12_aprov_inv
        APPENDING CORRESPONDING FIELDS OF TABLE tg_log
        WHERE kostl = wa_saida-kostl
        AND   lote  = wa_saida-lote.

*      IF WA_SAIDA-TXTSTAT = 'Aprovado' AND TG_LOG[] IS NOT INITIAL.
*        SELECT *
*         FROM ZIM12_APROV
*         INTO TABLE @DATA(IT_ZIM12_APROV)
*         WHERE   KOSTL = @WA_SAIDA-KOSTL.
*        LOOP AT IT_ZIM12_APROV INTO DATA(W_ZIM12).
*          READ TABLE TG_LOG WITH KEY KOSTL = W_ZIM12-KOSTL
*                                     NIVEL = W_ZIM12-NIVEL.
*          IF SY-SUBRC NE 0.
*            READ TABLE TG_LOG INTO DATA(WLOG)
*                    WITH KEY APROVADOR = W_ZIM12-APROVADOR.
*            IF SY-SUBRC = 0.
*              MOVE-CORRESPONDING WLOG TO TG_LOG.
*              WLOG-NIVEL = W_ZIM12-NIVEL.
*              WLOG-FASE = W_ZIM12-FASE.
*              APPEND WLOG TO TG_LOG.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.

      LOOP AT tg_log.
        IF tg_log-kostl IS INITIAL.
          tg_log-kostl = ' AVAL. T.I'.
          MODIFY tg_log INDEX sy-tabix TRANSPORTING kostl.
        ENDIF.
      ENDLOOP.
      SORT tg_log BY kostl nivel.
      CALL SCREEN 0200 STARTING AT 050 3
                       ENDING   AT 150 12.
    WHEN '&IC1'.
      READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.

      IF sy-subrc EQ 0.
* Se foi clicado na coluna EBELN.
        IF selfield-fieldname = 'KOSTL'.
* Passa o valor clicado na coluna como parâmetro para a transação
          SET PARAMETER ID 'KOS' FIELD wa_saida-kostl.
          SET PARAMETER ID 'GJR' FIELD wa_saida-ano.
          SET PARAMETER ID 'MEN' FIELD sy-repid.
* Chamo a transação

          auth_object = 'ZIM00'.
          auth_field1 = 'ACTVT'.
          auth_value1 = '5'.

          auth_user = sy-uname.

          w_val = sy-uname.

          SELECT SINGLE valfrom FROM setleaf
            INTO w_val
            WHERE setname =  'ZIM05_USUARIOS_IM' AND
                  valfrom = w_val.

          IF sy-subrc = 0.
            CALL TRANSACTION 'ZIM05' AND SKIP FIRST SCREEN.
          ELSE.
            CALL TRANSACTION 'ZIM02' AND SKIP FIRST SCREEN.
          ENDIF.


        ENDIF.
      ENDIF.
    WHEN 'SAVE'.
    WHEN 'VOLTA'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'ECAN'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDFORM. "COMANDO



*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
FORM definir_eventos.
  PERFORM f_carregar_eventos USING:
                                 slis_ev_user_command 'COMANDO'.
ENDFORM.                    " DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  f_retrieve_changed_data
*&---------------------------------------------------------------------*
FORM f_retrieve_changed_data .

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = cl_alv.
  CALL METHOD cl_alv->check_changed_data.

ENDFORM. " F_RETRIEVE_CHANGED_DATA
"F_BACK
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.

  wa_layout-zebra      = 'X'.
  wa_layout-no_toolbar = 'X'.
  wa_layout-no_rowmark = 'X'.
  wa_stable-row        = ' '.
  wa_layout-grid_title = ' '.

  "GRID1
  IF obg_conteiner_estra IS INITIAL.
    CREATE OBJECT obg_conteiner_estra
      EXPORTING
        container_name = g_cc_estra.


    CREATE OBJECT grid1
      EXPORTING
        i_parent = obg_conteiner_estra.


    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
*    WA_LAYOUT-GRID_TITLE = 'Estratégia de Liberação'.
    wa_layout-grid_title = TEXT-t05.
    wa_layout-no_toolbar = 'X'.
    PERFORM montar_layout_estra.




    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
*       IT_FILTER            = TL_FILTER
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_estra[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_estra .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:

         1 'ZIM12_APROV' 'NIVEL'       'TG_ESTRA'    'NIVEL'              TEXT-a04               '05' ' ' ' ' ' ',
         1 'ZIM12_APROV' 'APROVADOR'   'TG_ESTRA'    'APROVADOR'          TEXT-a03               '20' ' ' ' ' ' ',
         1 ' '           ' '           'TG_ESTRA'    'ESTADO'             TEXT-a06               '10' ' ' ' ' ' ',
         1 ' '           ' '           'TG_ESTRA'    'OPCOES'             TEXT-a07               '12' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'DATA_APROV'  'TG_ESTRA'    'DATA_APROV'     'Data'                 '10' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'HORA_APROV'  'TG_ESTRA'    'HORA_APROV'     'Hora'                 '10' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'TOTAL'       'TG_ESTRA'    'TOTAL'          'Vlr.Total'            '15' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'TOTAL'       'TG_ESTRA'    'TOTAL_USD'      'Vlr.Total USD'        '15' ' ' ' ' ' '.

ENDFORM.

FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            p_scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

*  IF P_FIELD EQ 'OPCOES'. "
*    W_FIELDCATALOG-HOTSPOT = 'X'.
*  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok-code.
    WHEN 'SAIR' OR 'EXIT' OR 'CANCEL'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos2 OUTPUT.
  wa_layout-zebra      = 'X'.
  wa_layout-no_toolbar = 'X'.
  wa_layout-no_rowmark = 'X'.
  wa_stable-row        = ' '.
  wa_layout-grid_title = ' '.

  "GRID2
  IF obg_conteiner_log IS INITIAL.
    CREATE OBJECT obg_conteiner_log
      EXPORTING
        container_name = g_cc_log.


    CREATE OBJECT grid2
      EXPORTING
        i_parent = obg_conteiner_log.


    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
*    WA_LAYOUT-GRID_TITLE = 'Estratégia de Liberação'.
    wa_layout-grid_title = TEXT-t05.
    wa_layout-no_toolbar = 'X'.
    PERFORM montar_layout_log.




    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
*       IT_FILTER            = TL_FILTER
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_log[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_log .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:

         1 ' '               ' '           'TG_LOG'    'KOSTL'              'Centro Custo'         '15' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'NIVEL'       'TG_LOG'    'NIVEL'              'Nível'                '05' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'APROVADOR'   'TG_LOG'    'APROVADOR'          'Aprovador'            '20' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'DATA_APROV'  'TG_LOG'    'DATA_APROV'         'Data'                 '10' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'HORA_APROV'  'TG_LOG'    'HORA_APROV'         'Hora'                 '10' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'TOTAL'       'TG_LOG'    'TOTAL'              'Vlr.Total'            '15' ' ' ' ' ' ',
         1 'ZIM12_APROV_INV' 'TOTAL'       'TG_LOG'    'TOTAL_USD'          'Vlr.Total USD'        '15' ' ' ' ' ' '.

ENDFORM.
