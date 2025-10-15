*&---------------------------------------------------------------------*
*& Report  ZMMR126
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr126 MESSAGE-ID zcarga.

TABLES: zde_zsdt0001cg_alv,
        zde_zsdt0001od_alv,
        zde_zsdt0001nt_alv,
        zde_zsdt0001ov_alv,
        zde_zsdt0001acb_alv.

CLASS lcl_event_receiver DEFINITION DEFERRED.
CLASS lcl_event_receiver_est DEFINITION DEFERRED.

*****************************************************************************
*****************************************************************************

DATA: it_retorno              TYPE zde_zsdt0001cg_alv_t,
      ck_selecionou           TYPE char01,
      ck_confer_carga         TYPE char01,
      ck_dado_frete_9011      TYPE char01,
      ck_erro_frete_9011      TYPE char01,
      ck_erro_pedagio_9011    TYPE char01,
      gb_id_carga             TYPE zde_id_carga,
      gb_st_carga             TYPE char01,
      gb_id_carga_est         TYPE zde_id_carga,
      gb_st_carga_est         TYPE char01,
      it_mensagens            TYPE zde_ob_mensagem_t,
      it_notas                TYPE TABLE OF zde_zsdt0001nt_alv WITH HEADER LINE,
      wa_nota_selecionada     TYPE zde_zsdt0001nt_alv,
      it_takes_saldo          TYPE TABLE OF zde_zsdt0001tk_alv_vinc WITH HEADER LINE,
      it_takes_vincu          TYPE TABLE OF zde_zsdt0001tk_alv WITH HEADER LINE,
      it_ordens_venda         TYPE TABLE OF zde_zsdt0001ov_alv WITH HEADER LINE,
      it_pedido_compra        TYPE TABLE OF zde_zsdt0001ek_alv WITH HEADER LINE,
      ex_parceiros            TYPE REF TO zcx_parceiros,
      ex_carga                TYPE REF TO zcx_carga,
      ex_cadastro             TYPE REF TO zcx_cadastro,
      ex_romaneio             TYPE REF TO zcx_romaneio,
      ex_ordem                TYPE REF TO zcx_ordem_carregamento,
      ex_ordem_venda          TYPE REF TO zcx_ordem_venda,
      ex_soft_expert_workflow TYPE REF TO zcx_soft_expert_workflow,
      ex_job                  TYPE REF TO zcx_job,
      ex_pedido               TYPE REF TO zcx_pedido_compra_exception,
      ex_miro                 TYPE REF TO zcx_miro_exception,
      go_alarm                TYPE REF TO lcl_event_receiver,
      go_clock                TYPE REF TO cl_gui_timer,
      go_alarm_est            TYPE REF TO lcl_event_receiver_est,
      go_clock_est            TYPE REF TO cl_gui_timer,
      lc_zcl_job              TYPE REF TO zcl_job,
      lc_zcl_job_est          TYPE REF TO zcl_job,
      vg_text(16)             TYPE c,
      vg_ord_ext,
      screen_tela             TYPE sy-dynnr.

TYPES: BEGIN OF ty_itens_alv.
         INCLUDE STRUCTURE zde_zsdt0001cg_alv.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
         ico_carga     TYPE char04,
       END OF ty_itens_alv.

*****************************************************************************
*****************************************************************************

*---------------------------------------------------------------------*
*       CLASS c_service DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS c_service DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_pic_tab             IMPORTING mime_url TYPE csequence EXPORTING pic_tab  TYPE STANDARD TABLE.
    CLASS-METHODS verifica_filtro_initial RETURNING VALUE(r_vazio) TYPE char01.
    CLASS-METHODS verifica_filtro_initial_e RETURNING VALUE(r_vazio) TYPE char01.
ENDCLASS.                    "c_service DEFINITION

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_finished FOR EVENT finished OF cl_gui_timer.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_event_receiver_est DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_finished FOR EVENT finished OF cl_gui_timer.
  PRIVATE SECTION.
ENDCLASS.

CONSTANTS: cs_line_color_finalizado  TYPE c LENGTH 4 VALUE 'C500',
           cs_line_color_alterado    TYPE c LENGTH 4 VALUE 'C300',
           cs_line_color_selecionada TYPE c LENGTH 4 VALUE 'C601'.

DATA: lc_filtro TYPE zde_filtro_zsdt0001cg.

DATA: ctl_cccontainer_picture TYPE REF TO cl_gui_container,
      splitter                TYPE REF TO cl_gui_splitter_container,
      picture                 TYPE REF TO cl_gui_picture,
      objeto                  TYPE REF TO zif_carga.

TYPES: BEGIN OF ty_add_nfe.
TYPES: n55_chave_acesso	TYPE zde_chave_doc_e,
       docnum_nfe       TYPE j_1bdocnum,
       n55_stat_sefaz	  TYPE j_1bstatuscode,
       dt_emissao       TYPE zde_zsdt0001nt_alv-dt_emissao,
       numero           TYPE zde_zsdt0001nt_alv-nr_nota,
       serie            TYPE zde_zsdt0001nt_alv-nm_serie,
       bukrs            TYPE bukrs,
       branch	          TYPE j_1bbranc_,
       parid            TYPE j_1bparid, "Ajuda de Pesquisa DEBI_KRED
       parid_ie         TYPE zde_ie,
       butxt            TYPE butxt,
       name	            TYPE name1,
       name1            TYPE name1_gp,
       nftot            TYPE j_1bnftot,
       ntgew            TYPE ntgew_15,
       ck_incluir       TYPE char01,
       cfop             TYPE zde_zsdt0001nt_alv-cfop.
TYPES: END OF ty_add_nfe.

DATA: wa_add_nfe_9002 TYPE ty_add_nfe.

DATA: ok_code   TYPE sy-ucomm,
      nm_report TYPE zbit0003-ds_nome_tecnico,
      nm_atual  TYPE char01.

SELECTION-SCREEN BEGIN OF SCREEN 0101 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK cte WITH FRAME TITLE TEXT-108.
    SELECT-OPTIONS: eidcarga FOR zde_zsdt0001cg_alv-id_carga,
                    erroment FOR zde_zsdt0001nt_alv-nr_romaneio_ent,
                    erromsai FOR zde_zsdt0001ov_alv-nr_romaneio_sai,
                    eidordem FOR zde_zsdt0001cg_alv-id_ordem,
                    eidentra FOR zde_zsdt0001nt_alv-id_entrada,
                    eidlocal FOR zde_zsdt0001cg_alv-id_local_entrega,
                    edtmovim FOR zde_zsdt0001cg_alv-dt_movimento,
                    enrsafra FOR zde_zsdt0001cg_alv-nr_safra  NO-DISPLAY,
                    eidbukrs FOR zde_zsdt0001cg_alv-id_bukrs  NO-DISPLAY,
                    eidbranc FOR zde_zsdt0001cg_alv-id_branch NO-DISPLAY,
                    eidagent FOR zde_zsdt0001cg_alv-id_agent_frete,
                    eidcolet FOR zde_zsdt0001cg_alv-id_local_coleta,
                    eiddesti FOR zde_zsdt0001cg_alv-id_local_destino,
                    eiddesca FOR zde_zsdt0001cg_alv-id_local_descarga,
                    eidprodu FOR zde_zsdt0001cg_alv-id_produto,
                    edstrato FOR zde_zsdt0001cg_alv-ds_placa_trator,
                    edsrebo1 FOR zde_zsdt0001cg_alv-ds_placa_reboq_1,
                    edsrebo2 FOR zde_zsdt0001cg_alv-ds_placa_reboq_2,
                    edsrebo3 FOR zde_zsdt0001cg_alv-ds_placa_reboq_3,
                    eidmotor FOR zde_zsdt0001cg_alv-id_motorista,
                    enrticke FOR zde_zsdt0001cg_alv-nr_ticket,
                    etpstatu FOR zde_zsdt0001cg_alv-tp_status,
                    edtabert FOR zde_zsdt0001cg_alv-dt_abertura,
                    ehrabert FOR zde_zsdt0001cg_alv-hr_abertura,
                    edtfecha FOR zde_zsdt0001cg_alv-dt_fechamento,
                    ehrfecha FOR zde_zsdt0001cg_alv-hr_fechamento.
  SELECTION-SCREEN END OF BLOCK cte.
SELECTION-SCREEN END OF SCREEN 0101.

SELECT-OPTIONS: iidcarga FOR zde_zsdt0001cg_alv-id_carga NO-DISPLAY,
                iidordem FOR zde_zsdt0001cg_alv-id_ordem NO-DISPLAY,
                iidentra FOR zde_zsdt0001nt_alv-id_entrada NO-DISPLAY,
                iidlocal FOR zde_zsdt0001cg_alv-id_local_entrega NO-DISPLAY,
                idtmovim FOR zde_zsdt0001cg_alv-dt_movimento NO-DISPLAY,
                inrsafra FOR zde_zsdt0001cg_alv-nr_safra NO-DISPLAY,
                iidbukrs FOR zde_zsdt0001cg_alv-id_bukrs NO-DISPLAY,
                iidbranc FOR zde_zsdt0001cg_alv-id_branch NO-DISPLAY,
                iidagent FOR zde_zsdt0001cg_alv-id_agent_frete NO-DISPLAY,
                iidcolet FOR zde_zsdt0001cg_alv-id_local_coleta NO-DISPLAY,
                iiddesti FOR zde_zsdt0001cg_alv-id_local_destino NO-DISPLAY,
                iiddesca FOR zde_zsdt0001cg_alv-id_local_descarga NO-DISPLAY,
                iidprodu FOR zde_zsdt0001cg_alv-id_produto NO-DISPLAY,
                idstrato FOR zde_zsdt0001cg_alv-ds_placa_trator NO-DISPLAY,
                idsrebo1 FOR zde_zsdt0001cg_alv-ds_placa_reboq_1 NO-DISPLAY,
                idsrebo2 FOR zde_zsdt0001cg_alv-ds_placa_reboq_2 NO-DISPLAY,
                idsrebo3 FOR zde_zsdt0001cg_alv-ds_placa_reboq_3 NO-DISPLAY,
                iidmotor FOR zde_zsdt0001cg_alv-id_motorista NO-DISPLAY,
                itpstatu FOR zde_zsdt0001cg_alv-tp_status NO-DISPLAY,
                inrticke FOR zde_zsdt0001cg_alv-nr_ticket NO-DISPLAY,
                idtabert FOR zde_zsdt0001cg_alv-dt_abertura NO-DISPLAY,
                ihrabert FOR zde_zsdt0001cg_alv-hr_abertura NO-DISPLAY,
                idtfecha FOR zde_zsdt0001cg_alv-dt_fechamento NO-DISPLAY,
                ihrfecha FOR zde_zsdt0001cg_alv-hr_fechamento NO-DISPLAY,
                irroment FOR zde_zsdt0001nt_alv-nr_romaneio_ent NO-DISPLAY,
                irromsai FOR zde_zsdt0001ov_alv-nr_romaneio_sai NO-DISPLAY.

"Seleção Padrão
PARAMETERS: ptipca   TYPE zde_tp_carga      NO-DISPLAY,
            psafra   TYPE zde_nr_safra      NO-DISPLAY,
            pempre   TYPE zde_bukrs_receb   NO-DISPLAY,
            pfilia   TYPE zde_branch_receb  NO-DISPLAY,
            pmanut   TYPE char01            NO-DISPLAY,
            pck_cad  TYPE char01            NO-DISPLAY,
            pidcarga TYPE zde_id_carga      NO-DISPLAY,
            pidsolic TYPE zde_id_sol_ajuste NO-DISPLAY.

INCLUDE zmmr126_status_0200.
INCLUDE zmmr126_user_command_0001.
INCLUDE zmmr126_status_9007.
INCLUDE zmmr126_status_9004.
INCLUDE zmmr126_status_0312.
INCLUDE zmmr126_status_0313.
INCLUDE zmmr126_status_0300.
INCLUDE zmmr126_status_9001.
INCLUDE zmmr126_status_9002.
INCLUDE zmmr126_status_9003.
INCLUDE zmmr126_atribui_0308.

INITIALIZATION.

  SELECT SINGLE * INTO @DATA(wa_centro_a_fixar)
    FROM zsdt_depara_cen
   WHERE tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_a_fixar.

  IF sy-subrc IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_zmmt0017)
      FROM zmmt0017.

    SORT it_zmmt0017 BY centro_fixo centro_a_fixar.
    DELETE ADJACENT DUPLICATES FROM it_zmmt0017 COMPARING centro_fixo centro_a_fixar.

    LOOP AT it_zmmt0017 INTO DATA(wa_zmmt0017).
      UPDATE zsdt_depara_cen
         SET tp_centro_virtual = zcl_pedido_compra=>st_tp_centro_a_fixar
       WHERE centro_real EQ wa_zmmt0017-centro_fixo
         AND centrov_1   EQ wa_zmmt0017-centro_a_fixar.
    ENDLOOP.
    COMMIT WORK.
  ENDIF.

START-OF-SELECTION.

  DATA: wa_carga TYPE zsdt0001cg,
        lc_ok    TYPE char01.

  IF ptipca  IS INITIAL OR
     psafra  IS INITIAL OR
     pempre  IS INITIAL OR
     pfilia  IS INITIAL.
    PERFORM busca_ultimo_acesso USING lc_ok.
    IF lc_ok EQ abap_false.
      PERFORM selecao_padrao.
    ENDIF.
  ENDIF.

  IF pck_cad EQ abap_true.
    CLEAR: zde_zsdt0001cg_alv,
           zde_zsdt0001od_alv,
           zde_zsdt0001nt_alv.

    CASE ptipca.
      WHEN zif_carga=>st_tp_carga_entrada_fob.
        CREATE OBJECT objeto TYPE zcl_carga_recebimento.
      WHEN zif_carga=>st_tp_carga_saida_opus.
        CREATE OBJECT objeto TYPE zcl_carga_saida_opus.
      WHEN zif_carga=>st_tp_carga_saida_ent_fob.
        CREATE OBJECT objeto TYPE zcl_carga_saida.
    ENDCASE.

    IF pidcarga IS NOT INITIAL.
      TRY .
          IF pmanut EQ abap_false.
            objeto->set_registro( i_id_carga = pidcarga ).
          ELSE.
            IF pidsolic IS INITIAL.
              objeto->set_cria_manutencao( i_id_carga = pidcarga ).
            ELSE.
              objeto->set_registro_manutencao( i_id_solicitacao = pidsolic ).
            ENDIF.
          ENDIF.
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          PERFORM limpar_tela.
          LEAVE PROGRAM.
        CATCH zcx_ordem_carregamento INTO ex_ordem_carregamento.
          ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          PERFORM limpar_tela.
          LEAVE PROGRAM.
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          PERFORM limpar_tela.
          LEAVE PROGRAM.
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          PERFORM limpar_tela.
          LEAVE PROGRAM.
        CATCH zcx_soft_expert_workflow INTO ex_soft_expert_workflow.
          ex_soft_expert_workflow->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          PERFORM limpar_tela.
          LEAVE PROGRAM.
      ENDTRY.

      objeto->get_info_alv_apresentacao( IMPORTING e_apresentacao = e_apresentacao ).
      zde_zsdt0001cg_alv  = e_apresentacao-carga.
      zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
      zde_zsdt0001acb_alv = e_apresentacao-manutencao.
      it_notas[]          = e_apresentacao-notas[].
      it_ordens_venda[]   = e_apresentacao-ordem_venda[].
      it_pedido_compra[]  = e_apresentacao-pedido_compra[].
      it_takes_vincu[]    = e_apresentacao-takeup[].
    ELSE.
      TRY .
          objeto->novo_registro( )->set_abrir(
            EXPORTING
              i_nr_safra             = psafra    " Safra
              i_id_bukrs             = pempre    " Empresa Recebimento
              i_id_branch            = pfilia    " Filial de Recebimento
            IMPORTING
              e_carga_recebimento    = zde_zsdt0001cg_alv " Cargas (Romaneios) - ALV - PESQUISA
          ).
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          PERFORM limpar_tela.
          LEAVE PROGRAM.
        CATCH zcx_ordem_carregamento INTO ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          PERFORM limpar_tela.
          LEAVE PROGRAM.
      ENDTRY.
    ENDIF.
    CLEAR: ck_confer_carga.
    CALL SCREEN 0300.

    IF pidcarga IS INITIAL AND ck_confer_carga EQ abap_true AND pmanut EQ abap_false.
      TRY.
          CLEAR: lc_filtro.
          objeto->get_registro( IMPORTING e_registro = wa_carga )->free( ).
          lc_filtro-iidcarga = VALUE #( option = 'EQ' sign = 'I' ( low = wa_carga-id_carga  high = wa_carga-id_carga  ) ).
          lc_filtro-inrsafra = VALUE #( option = 'EQ' sign = 'I' ( low = wa_carga-nr_safra  high = wa_carga-nr_safra  ) ).
          lc_filtro-iidbukrs = VALUE #( option = 'EQ' sign = 'I' ( low = wa_carga-id_bukrs  high = wa_carga-id_bukrs  ) ).
          lc_filtro-iidbranc = VALUE #( option = 'EQ' sign = 'I' ( low = wa_carga-id_branch high = wa_carga-id_branch ) ).
          objeto->pesquisar( EXPORTING i_filtros = lc_filtro IMPORTING e_registros = it_retorno e_pesquisou = DATA(e_pesquisou) ).
          IF e_pesquisou EQ abap_true.
            CALL SCREEN 0200.
          ENDIF.
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.
    ENDIF.

    PERFORM limpar_tela.

    LEAVE PROGRAM.
  ENDIF.

  IF c_service=>verifica_filtro_initial( ) EQ abap_true.
    PERFORM ajusta_tabela_tipo_entrada.
    CALL SCREEN 0001.
  ELSE.

    CASE ptipca.
      WHEN zif_carga=>st_tp_carga_entrada_fob.
        CREATE OBJECT objeto TYPE zcl_carga_recebimento.
      WHEN zif_carga=>st_tp_carga_saida_opus.
        CREATE OBJECT objeto TYPE zcl_carga_saida_opus.
      WHEN zif_carga=>st_tp_carga_saida_ent_fob.
        CREATE OBJECT objeto TYPE zcl_carga_saida.
    ENDCASE.

    "Pesquisar
    TRY.
        CLEAR: lc_filtro.
        lc_filtro-iidcarga[] = iidcarga[].
        lc_filtro-iidordem[] = iidordem[].
        lc_filtro-iidentra[] = iidentra[].
        lc_filtro-iidlocal[] = iidlocal[].
        lc_filtro-idtmovim[] = idtmovim[].
        lc_filtro-inrsafra[] = inrsafra[].
        lc_filtro-iidbukrs[] = iidbukrs[].
        lc_filtro-iidbranc[] = iidbranc[].
        lc_filtro-iidagent[] = iidagent[].
        lc_filtro-iidcolet[] = iidcolet[].
        lc_filtro-iiddesti[] = iiddesti[].
        lc_filtro-iiddesca[] = iiddesca[].
        lc_filtro-iidprodu[] = iidprodu[].
        lc_filtro-idstrato[] = idstrato[].
        lc_filtro-idsrebo1[] = idsrebo1[].
        lc_filtro-idsrebo2[] = idsrebo2[].
        lc_filtro-idsrebo3[] = idsrebo3[].
        lc_filtro-iidmotor[] = iidmotor[].
        lc_filtro-inrticke[] = inrticke[].
        lc_filtro-itpstatu[] = itpstatu[].
        lc_filtro-idtabert[] = idtabert[].
        lc_filtro-ihrabert[] = ihrabert[].
        lc_filtro-idtfecha[] = idtfecha[].
        lc_filtro-ihrfecha[] = ihrfecha[].
        lc_filtro-irroment[] = irroment[].
        lc_filtro-irromsai[] = irromsai[].

        objeto->pesquisar( EXPORTING i_filtros   = lc_filtro IMPORTING e_registros = it_retorno e_pesquisou = e_pesquisou ).
        IF e_pesquisou EQ abap_true.
          CALL SCREEN 0200.
        ENDIF.
      CATCH zcx_carga INTO ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    ENDTRY.

    PERFORM limpar_tela.
    LEAVE PROGRAM.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  IF c_service=>verifica_filtro_initial_e( ) EQ abap_false.

    enrsafra[] = VALUE #( option = 'EQ' sign = 'I' ( low = psafra high = psafra ) ).
    eidbukrs[] = VALUE #( option = 'EQ' sign = 'I' ( low = pempre high = pempre ) ).
    eidbranc[] = VALUE #( option = 'EQ' sign = 'I' ( low = pfilia high = pfilia ) ).

    SUBMIT zmmr126 WITH iidcarga IN eidcarga
                   WITH iidordem IN eidordem
                   WITH iidentra IN eidentra
                   WITH iidlocal IN eidlocal
                   WITH idtmovim IN edtmovim
                   WITH inrsafra IN enrsafra
                   WITH iidbukrs IN eidbukrs
                   WITH iidbranc IN eidbranc
                   WITH iidagent IN eidagent
                   WITH iidcolet IN eidcolet
                   WITH iiddesti IN eiddesti
                   WITH iiddesca IN eiddesca
                   WITH iidprodu IN eidprodu
                   WITH idstrato IN edstrato
                   WITH idsrebo1 IN edsrebo1
                   WITH idsrebo2 IN edsrebo2
                   WITH idsrebo3 IN edsrebo3
                   WITH iidmotor IN eidmotor
                   WITH inrticke IN enrticke
                   WITH itpstatu IN etpstatu
                   WITH idtabert IN edtabert
                   WITH ihrabert IN ehrabert
                   WITH idtfecha IN edtfecha
                   WITH ihrfecha IN ehrfecha
                   WITH irroment IN erroment
                   WITH irromsai IN erromsai
                   WITH psafra   EQ psafra
                   WITH pempre   EQ pempre
                   WITH pfilia   EQ pfilia
                    AND RETURN.
  ENDIF.
  "  CLEAR: zib_nfe_dist_ter-chave_nfe.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF edtabert[] IS INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = sy-datlo high = sy-datlo ) TO edtabert.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  DATA: e_uri    TYPE string,
        c_uri    TYPE c LENGTH 400,
        it_ucomm TYPE TABLE OF sy-ucomm.

  CLEAR: it_ucomm[], it_ucomm.

  CASE ptipca.
    WHEN zif_carga=>st_tp_carga_entrada_fob.
      APPEND 'AUTOMATICO' TO it_ucomm.
    WHEN zif_carga=>st_tp_carga_saida_opus.
      APPEND 'NOVA_CARGA' TO it_ucomm.
      APPEND 'NOVA_SOLIC' TO it_ucomm.
      APPEND 'AUTOMATICO' TO it_ucomm.
    WHEN zif_carga=>st_tp_carga_saida_ent_fob.
      APPEND 'NOVA_CARGA' TO it_ucomm.
      APPEND 'NOVA_SOLIC' TO it_ucomm.
  ENDCASE.

  SET PF-STATUS 'PF0001' EXCLUDING it_ucomm.

  SELECT SINGLE * INTO @DATA(wa_local)
    FROM j_1bbranch
   WHERE bukrs  EQ @pempre
     AND branch EQ @pfilia.

  CONCATENATE wa_local-branch '-' wa_local-name INTO DATA(tx_local).

  CASE ptipca.
    WHEN zif_carga=>st_tp_carga_entrada_fob.
      SET TITLEBAR 'TL0001' WITH psafra tx_local.
    WHEN zif_carga=>st_tp_carga_saida_opus.
      SET TITLEBAR 'TL0002' WITH psafra tx_local.
  ENDCASE.

  IF splitter IS INITIAL.

    CREATE OBJECT splitter
      EXPORTING
        parent  = cl_gui_container=>screen0 "CTL_CCCONTAINER
        rows    = 1
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = ctl_cccontainer_picture.

  ENDIF.

  IF picture IS INITIAL.

    CREATE OBJECT picture
      EXPORTING
        parent = ctl_cccontainer_picture
      EXCEPTIONS
        error  = 1.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_stretch
      EXCEPTIONS
        error        = 1.

    PERFORM load_pic_from_db USING picture.

  ENDIF.

ENDMODULE.

##PERF_NO_TYPE
FORM load_pic_from_db  USING  gui_picture TYPE REF TO cl_gui_picture.

  DATA url(255).
  TYPES pic_line(1022) TYPE x.
  DATA  pic_tab TYPE TABLE OF pic_line.

  CLEAR url.
  url = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'.

  c_service=>get_pic_tab(
        EXPORTING mime_url = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'
        IMPORTING pic_tab  = pic_tab ).

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type    = 'image'
      subtype = 'GIF'
    TABLES
      data    = pic_tab
    CHANGING
      url     = url
    EXCEPTIONS
      OTHERS  = 1.

  CALL METHOD gui_picture->load_picture_from_url
    EXPORTING
      url = url.

ENDFORM.                               " LOAD_PIC_FROM_DB

CLASS c_service IMPLEMENTATION.
  METHOD verifica_filtro_initial_e.
    IF NOT ( eidcarga[] IS INITIAL AND
             eidordem[] IS INITIAL AND
             eidentra[] IS INITIAL AND
             eidlocal[] IS INITIAL AND
             edtmovim[] IS INITIAL AND
             enrsafra[] IS INITIAL AND
             eidbukrs[] IS INITIAL AND
             eidbranc[] IS INITIAL AND
             eidagent[] IS INITIAL AND
             eidcolet[] IS INITIAL AND
             eiddesti[] IS INITIAL AND
             eiddesca[] IS INITIAL AND
             eidprodu[] IS INITIAL AND
             edstrato[] IS INITIAL AND
             edsrebo1[] IS INITIAL AND
             edsrebo2[] IS INITIAL AND
             edsrebo3[] IS INITIAL AND
             eidmotor[] IS INITIAL AND
             enrticke[] IS INITIAL AND
             etpstatu[] IS INITIAL AND
             edtabert[] IS INITIAL AND
             ehrabert[] IS INITIAL AND
             edtfecha[] IS INITIAL AND
             ehrfecha[] IS INITIAL AND
             erroment[] IS INITIAL AND
             erromsai[] IS INITIAL ).
      r_vazio = abap_false.
    ELSE.
      r_vazio = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD verifica_filtro_initial.
    IF NOT ( iidcarga[] IS INITIAL AND
             iidordem[] IS INITIAL AND
             iidentra[] IS INITIAL AND
             iidlocal[] IS INITIAL AND
             idtmovim[] IS INITIAL AND
             inrsafra[] IS INITIAL AND
             iidbukrs[] IS INITIAL AND
             iidbranc[] IS INITIAL AND
             iidagent[] IS INITIAL AND
             iidcolet[] IS INITIAL AND
             iiddesti[] IS INITIAL AND
             iiddesca[] IS INITIAL AND
             iidprodu[] IS INITIAL AND
             idstrato[] IS INITIAL AND
             idsrebo1[] IS INITIAL AND
             idsrebo2[] IS INITIAL AND
             idsrebo3[] IS INITIAL AND
             iidmotor[] IS INITIAL AND
             inrticke[] IS INITIAL AND
             itpstatu[] IS INITIAL AND
             idtabert[] IS INITIAL AND
             ihrabert[] IS INITIAL AND
             idtfecha[] IS INITIAL AND
             ihrfecha[] IS INITIAL AND
             irroment[] IS INITIAL AND
             irromsai[] IS INITIAL ).
      r_vazio = abap_false.
    ELSE.
      r_vazio = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_pic_tab.
    DATA pic_wa TYPE xstring.
    DATA length TYPE i.
    DATA mime_api TYPE REF TO if_mr_api.
    mime_api = cl_mime_repository_api=>get_api( ).
    mime_api->get( EXPORTING
                     i_url             = mime_url
                     i_check_authority = abap_false
                   IMPORTING
                     e_content = pic_wa
                   EXCEPTIONS OTHERS = 4 ).
    IF sy-subrc = 4.
      RETURN.
    ENDIF.
    CLEAR pic_tab.
    length = xstrlen( pic_wa ).
    WHILE length >= 1022.
      APPEND pic_wa(1022) TO pic_tab.
      SHIFT pic_wa BY 1022 PLACES LEFT IN BYTE MODE.
      length = xstrlen( pic_wa ).
    ENDWHILE.
    IF length > 0.
      APPEND pic_wa TO pic_tab.
    ENDIF.
  ENDMETHOD.                    "get_pic_tab

ENDCLASS.                    "c_service IMPLEMENTATION


CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: on_finished.

    DATA: lc_tempo TYPE c LENGTH 11.

    IF gb_st_carga EQ 'W'.
      TRY .
          IF lc_zcl_job IS INITIAL.
            zcl_job=>get_time_next_job(
              EXPORTING
                i_job_name = zcl_job=>st_name_job_entrada_estoque
              IMPORTING
                e_jobname  = DATA(e_jobname)
                e_jobcount = DATA(e_jobcount) ).

            CREATE OBJECT lc_zcl_job.
            lc_zcl_job->set_key_job( EXPORTING i_jobname = e_jobname i_jobcount = e_jobcount ).

          ENDIF.

          lc_zcl_job->get_job_registro_info( IMPORTING e_texto_info = DATA(e_texto_info) e_atrasado = DATA(e_atrasado) ).

          IF e_atrasado NE abap_true.
            MESSAGE e_texto_info TYPE 'S'.
          ELSE.
            MESSAGE e_texto_info TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.

        CATCH zcx_job.    "
      ENDTRY.
    ENDIF.

    IF go_clock IS NOT INITIAL.
      go_clock->interval = 5.
      go_clock->run(
        EXCEPTIONS
          error  = 1
          OTHERS = 2
      ).
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.

  ENDMETHOD.                           " ON_FINISHED
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION


CLASS lcl_event_receiver_est IMPLEMENTATION.
  METHOD: on_finished.

    DATA: lc_tempo TYPE c LENGTH 11.

    IF gb_st_carga_est EQ 'W'.
      TRY .

          IF lc_zcl_job_est IS INITIAL.

            zcl_job=>get_time_next_job(
                EXPORTING
                   i_job_name = zcl_job=>st_name_job_estorno_estoque
                IMPORTING
                   e_jobname = DATA(e_jobname)
                   e_jobcount = DATA(e_jobcount) ).

            CREATE OBJECT lc_zcl_job_est.
            lc_zcl_job_est->set_key_job( EXPORTING i_jobname = e_jobname i_jobcount = e_jobcount ).
          ENDIF.

          lc_zcl_job_est->get_job_registro_info( IMPORTING e_texto_info = DATA(e_texto_info) e_atrasado = DATA(e_atrasado) ).

          IF e_atrasado NE abap_true.
            MESSAGE e_texto_info TYPE 'S'.
          ELSE.
            MESSAGE e_texto_info TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.

        CATCH zcx_job.    "
      ENDTRY.
    ENDIF.

    IF go_clock_est IS NOT INITIAL.
      go_clock_est->interval = 5.
      go_clock_est->run(
        EXCEPTIONS
          error  = 1
          OTHERS = 2
      ).
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.

  ENDMETHOD.                           " ON_FINISHED
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

FORM limpar_tela.

  CLEAR: objeto.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECAO_PADRAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecao_padrao .
  ck_selecionou = abap_false.

  screen_tela = 9001.

  CALL SCREEN 9010.

  IF ck_selecionou EQ abap_false.
    LEAVE PROGRAM.
  ELSE.
    CLEAR: enrsafra, eidbukrs, eidbranc.
    enrsafra-sign   = 'I'.
    enrsafra-option = 'EQ'.
    enrsafra-high   = psafra.
    enrsafra-low    = psafra.
    APPEND enrsafra.

    eidbukrs-sign   = 'I'.
    eidbukrs-option = 'EQ'.
    eidbukrs-high   = pempre.
    eidbukrs-low    = pempre.
    APPEND eidbukrs.

    eidbranc-sign   = 'I'.
    eidbranc-option = 'EQ'.
    eidbranc-high   = pfilia.
    eidbranc-low    = pfilia.
    APPEND eidbranc.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_TABELA_TIPO_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_tabela_tipo_entrada .

*  DATA: WA_ZSDT0001TETX TYPE ZSDT0001TETX.
*
*  SELECT CE~ID_ENTRADA,
*         CE~DS_ENTRADA
*    FROM ZSDT0001TE AS CE
*    INTO TABLE @DATA(IT_ZSDT0001TE).
*
*  SELECT *
*    FROM ZSDT0001TETX
*    INTO TABLE @DATA(IT_ZSDT0001TETX).
*
*  SORT IT_ZSDT0001TE BY ID_ENTRADA.
*  DELETE ADJACENT DUPLICATES FROM IT_ZSDT0001TE COMPARING ID_ENTRADA.
*
*  DESCRIBE TABLE IT_ZSDT0001TE   LINES DATA(LC_QTD_LINHAS1).
*  DESCRIBE TABLE IT_ZSDT0001TETX LINES DATA(LC_QTD_LINHAS2).
*
*  IF LC_QTD_LINHAS1 NE LC_QTD_LINHAS2.
*
*    DELETE FROM ZSDT0001TETX.
*
*    CLEAR: IT_ZSDT0001TETX[], IT_ZSDT0001TETX.
*
*    LOOP AT IT_ZSDT0001TE INTO DATA(WA_ZSDT0001TE).
*      WA_ZSDT0001TETX-ID_ENTRADA = WA_ZSDT0001TE-ID_ENTRADA.
*      WA_ZSDT0001TETX-DS_ENTRADA = WA_ZSDT0001TE-DS_ENTRADA.
*      APPEND WA_ZSDT0001TETX TO IT_ZSDT0001TETX.
*    ENDLOOP.
*
*    IF IT_ZSDT0001TETX[] IS NOT INITIAL.
*      MODIFY ZSDT0001TETX FROM TABLE IT_ZSDT0001TETX[].
*      COMMIT WORK.
*    ENDIF.
*
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  NOVA_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nova_carga .
  SUBMIT zmmr126 WITH pck_cad EQ abap_true
                 WITH psafra   EQ psafra
                 WITH pempre   EQ pempre
                 WITH pfilia   EQ pfilia AND RETURN.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETORNO_ALV_ID_CARGA  text
*----------------------------------------------------------------------*
FORM mostrar_carga  USING p_id_carga TYPE zde_zsdt0001cg_alv-id_carga CHANGING p_ret_alv TYPE ty_itens_alv.

  DATA: obj_carga TYPE REF TO zif_carga.

  SUBMIT zmmr126 WITH pck_cad  EQ abap_true
                 WITH psafra   EQ psafra
                 WITH pempre   EQ pempre
                 WITH pfilia   EQ pfilia
                 WITH pidcarga EQ p_id_carga AND RETURN.

  CASE ptipca.
    WHEN zif_carga=>st_tp_carga_entrada_fob.
      CREATE OBJECT obj_carga TYPE zcl_carga_recebimento.
    WHEN zif_carga=>st_tp_carga_saida_opus.
      CREATE OBJECT obj_carga TYPE zcl_carga_saida_opus.
    WHEN zif_carga=>st_tp_carga_saida_ent_fob.
      CREATE OBJECT obj_carga TYPE zcl_carga_saida.
  ENDCASE.

  TRY .
      obj_carga->set_registro( i_id_carga = p_id_carga )->get_info_alv_apresentacao( IMPORTING e_apresentacao = DATA(e_apresentacao) ).
      MOVE-CORRESPONDING e_apresentacao-carga TO p_ret_alv.
      PERFORM seta_icone_status USING p_ret_alv-tp_status CHANGING p_ret_alv-ico_carga.
    CATCH zcx_carga.
    CATCH zcx_ordem_carregamento.
  ENDTRY.
  TRY .
      obj_carga->free( ).
    CATCH zcx_carga.
  ENDTRY.
  CLEAR: obj_carga.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERAR_ENTRADA_ESTOQUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_CARGA>  text
*----------------------------------------------------------------------*
FORM gerar_entrada_estoque USING p_id_carga TYPE zde_zsdt0001cg_alv-id_carga CHANGING p_ret_alv TYPE ty_itens_alv.

  DATA: obj_carga TYPE REF TO zif_carga.

  CASE ptipca.
    WHEN zif_carga=>st_tp_carga_entrada_fob.
      CREATE OBJECT obj_carga TYPE zcl_carga_recebimento.
    WHEN zif_carga=>st_tp_carga_saida_opus.
      CREATE OBJECT obj_carga TYPE zcl_carga_saida_opus.
    WHEN zif_carga=>st_tp_carga_saida_ent_fob.
      CREATE OBJECT obj_carga TYPE zcl_carga_saida.
  ENDCASE.

  TRY .

      obj_carga->set_registro( i_id_carga = p_id_carga
       )->set_gerar_romaneio_saida(
       )->set_gerar_entrada_estoque(
       )->set_processar_entrada(
       )->get_info_alv_apresentacao( IMPORTING e_apresentacao = DATA(e_apresentacao) ).

      MOVE-CORRESPONDING e_apresentacao-carga TO p_ret_alv.

    CATCH zcx_parceiros INTO ex_parceiros.
      ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_cadastro INTO ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_ordem_carregamento INTO ex_ordem_carregamento.
      ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_pedido_compra_exception INTO ex_pedido.
      ex_pedido->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_miro_exception INTO ex_miro.
      ex_miro->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_ordem_venda INTO ex_ordem_venda.
      ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_job INTO ex_job.
      ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.
  TRY .
      obj_carga->free( ).
    CATCH zcx_carga.
  ENDTRY.
  CLEAR: obj_carga.

  PERFORM chamar_atualizador USING e_apresentacao.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_ENTRADA_ESTOQUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_CARGA>  text
*----------------------------------------------------------------------*
FORM estornar_entrada_estoque USING p_id_carga TYPE zde_zsdt0001cg_alv-id_carga CHANGING p_ret_alv TYPE ty_itens_alv.

  DATA: obj_carga TYPE REF TO zif_carga.

  CASE ptipca.
    WHEN zif_carga=>st_tp_carga_entrada_fob.
      CREATE OBJECT obj_carga TYPE zcl_carga_recebimento.
    WHEN zif_carga=>st_tp_carga_saida_opus.
      CREATE OBJECT obj_carga TYPE zcl_carga_saida_opus.
    WHEN zif_carga=>st_tp_carga_saida_ent_fob.
      CREATE OBJECT obj_carga TYPE zcl_carga_saida.
  ENDCASE.

  TRY .

      obj_carga->set_registro( i_id_carga = p_id_carga
      )->set_gerar_estorno_estoque(
      )->set_processar_estorno(
      )->get_info_alv_apresentacao( IMPORTING e_apresentacao = DATA(e_apresentacao) ).

      MOVE-CORRESPONDING e_apresentacao-carga TO p_ret_alv.

    CATCH zcx_parceiros INTO ex_parceiros.
      ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_cadastro INTO ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_ordem_carregamento INTO ex_ordem_carregamento.
      ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_pedido_compra_exception INTO DATA(ex_pedido).
      ex_pedido->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_job INTO DATA(ex_job).
      ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

  TRY .
      obj_carga->free( ).
    CATCH zcx_carga.
  ENDTRY.
  CLEAR: obj_carga.

  PERFORM chamar_atualizador_estorno USING e_apresentacao.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_MSG_INTERFACE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CARGA_ROMANEIO_ID_CARGA  text
*----------------------------------------------------------------------*
FORM mostra_msg_interface  USING  p_id_carga TYPE zde_id_carga.

  DATA: obj_carga TYPE REF TO zif_carga.

  CASE ptipca.
    WHEN zif_carga=>st_tp_carga_entrada_fob.
      CREATE OBJECT obj_carga TYPE zcl_carga_recebimento.
    WHEN zif_carga=>st_tp_carga_saida_opus.
      CREATE OBJECT obj_carga TYPE zcl_carga_saida_opus.
    WHEN zif_carga=>st_tp_carga_saida_ent_fob.
      CREATE OBJECT obj_carga TYPE zcl_carga_saida.
  ENDCASE.

  TRY .
      obj_carga->get_mens_interface_entrada( EXPORTING i_id_carga  = p_id_carga IMPORTING e_mensagens = it_mensagens ).
      CALL SCREEN 9003 STARTING AT 40 05.
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

  TRY .
      obj_carga->free( ).
    CATCH zcx_carga.
  ENDTRY.

  CLEAR: obj_carga.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SETA_ICONE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RETORNO_ALV_TP_STATUS  text
*      <--P_IT_RETORNO_ALV_ICO_CARGA  text
*----------------------------------------------------------------------*
FORM seta_icone_status  USING    p_tp_status TYPE zde_status_carga
                        CHANGING p_ico_carga TYPE char04.

  CASE p_tp_status.
    WHEN zif_carga=>st_status_aberto.
      p_ico_carga = icon_import_transport_request.
    WHEN zif_carga=>st_status_fechado.
      p_ico_carga = icon_import_all_requests.
    WHEN zif_carga=>st_status_conferido.
      p_ico_carga = icon_transport_proposal.
    WHEN zif_carga=>st_status_cancelada.
      p_ico_carga = icon_terminated_position.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_ATUALIZADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RET_ALV  text
*----------------------------------------------------------------------*
FORM chamar_atualizador  USING p_carga TYPE zde_carga_apresentacao.

  DATA: ck_chamar TYPE c LENGTH 1.

  CHECK gb_id_carga IS INITIAL.

  ck_chamar = abap_false.

  LOOP AT p_carga-notas INTO DATA(wa_notas).
    IF wa_notas-obj_key_entrada IS NOT INITIAL AND wa_notas-docnum IS INITIAL.
      ck_chamar = abap_true.
    ENDIF.
  ENDLOOP.

  CHECK ck_chamar EQ abap_true.

  gb_id_carga = p_carga-carga-id_carga.
  TRY .
      CALL FUNCTION 'Z_VERIFICA_ENTRADA_STATUS'
        STARTING NEW TASK 'VERIFICA_STATUS'
        DESTINATION 'NONE'
        PERFORMING mostra_status ON END OF TASK
        EXPORTING
          id_carga = gb_id_carga.
    CATCH cx_root.
      CLEAR: gb_id_carga.
  ENDTRY.

  gb_st_carga = 'W'.

  PERFORM time USING abap_true.

ENDFORM.

FORM mostra_status USING taskname.

  DATA: r_status TYPE char01.

  RECEIVE RESULTS FROM FUNCTION 'Z_VERIFICA_ENTRADA_STATUS'
   IMPORTING r_status = r_status.

  PERFORM time USING abap_false.

  IF gb_id_carga EQ wa_carga_romaneio-id_carga.
    CLEAR: gb_id_carga.
    IF r_status EQ 'E' OR r_status EQ 'S'.
      gb_st_carga = r_status.
      IF obg_toolbar_2 IS NOT INITIAL.
        SET USER-COMMAND 'REFRESH_EN'.
      ENDIF.
    ELSE.
      CLEAR: gb_id_carga, gb_st_carga.
    ENDIF.
  ELSE.
    CLEAR: gb_id_carga, gb_st_carga.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_ATUALIZADOR_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RET_ALV  text
*----------------------------------------------------------------------*
FORM chamar_atualizador_estorno USING p_carga TYPE zde_carga_apresentacao.

  DATA: ck_chamar TYPE c LENGTH 1.

  CHECK gb_id_carga_est IS INITIAL.

  ck_chamar = abap_false.

  LOOP AT p_carga-notas INTO DATA(wa_notas).
    IF wa_notas-obj_key_entrada IS NOT INITIAL AND wa_notas-docnum IS NOT INITIAL.
      ck_chamar = abap_true.
    ENDIF.
  ENDLOOP.

  CHECK ck_chamar EQ abap_true.

  gb_id_carga_est = p_carga-carga-id_carga.
  TRY .
      CALL FUNCTION 'Z_VERIFICA_ESTORNO_STATUS'
        STARTING NEW TASK 'VERIFICA_STATUS_ESTORNO'
        DESTINATION 'NONE'
        PERFORMING mostra_status_estorno ON END OF TASK
        EXPORTING
          id_carga = gb_id_carga_est.
    CATCH cx_root.
      CLEAR: gb_id_carga_est.
  ENDTRY.

  gb_st_carga_est = 'W'.

  PERFORM time_estorno USING abap_true.

ENDFORM.

FORM mostra_status_estorno USING taskname.

  DATA: r_status TYPE char01.

  RECEIVE RESULTS FROM FUNCTION 'Z_VERIFICA_ESTORNO_STATUS'
   IMPORTING r_status = r_status.

  PERFORM time_estorno USING abap_false.

  IF gb_id_carga_est EQ wa_carga_romaneio-id_carga.
    CLEAR: gb_id_carga_est.
    IF r_status EQ 'E' OR r_status EQ 'S'.
      gb_st_carga_est = r_status.
      IF obg_toolbar_2 IS NOT INITIAL.
        SET USER-COMMAND 'REFRESH_ES'.
      ENDIF.
    ELSE.
      CLEAR: gb_id_carga_est, gb_st_carga_est.
    ENDIF.
  ELSE.
    CLEAR: gb_id_carga_est, gb_st_carga_est.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ATIVAR text
*----------------------------------------------------------------------*
FORM time  USING p_ativar TYPE char01.

  CASE p_ativar.
    WHEN abap_true.

      IF go_clock IS INITIAL.
        CREATE OBJECT: go_clock.
      ELSE.
        go_clock->cancel( ).
      ENDIF.

      IF go_alarm IS INITIAL.
        CREATE OBJECT: go_alarm.
        SET HANDLER go_alarm->on_finished FOR go_clock.
      ENDIF.

      go_clock->interval = 1.
      CALL METHOD go_clock->run.

    WHEN abap_false.

      CLEAR:
         go_alarm,
         go_clock,
         lc_zcl_job.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TIME_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ATIVAR text
*----------------------------------------------------------------------*
FORM time_estorno  USING p_ativar TYPE char01.

  CASE p_ativar.
    WHEN abap_true.

      IF go_clock_est IS INITIAL.
        CREATE OBJECT: go_clock_est.
      ELSE.
        go_clock_est->cancel( ).
      ENDIF.

      IF go_alarm_est IS INITIAL.
        CREATE OBJECT: go_alarm_est.
        SET HANDLER go_alarm_est->on_finished FOR go_clock_est.
      ENDIF.

      go_clock_est->interval = 1.
      CALL METHOD go_clock_est->run.

    WHEN abap_false.

      CLEAR:
         go_alarm_est,
         go_clock_est,
         lc_zcl_job_est.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_ULTIMO_ACESSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_ultimo_acesso USING p_ok TYPE char01.

  p_ok = abap_false.

  SELECT SINGLE * INTO @DATA(wa_zsdt0001us)
    FROM zsdt0001us
   WHERE us_name EQ @sy-uname.

  CHECK sy-subrc IS INITIAL.
  CHECK wa_zsdt0001us-id_bukrs IS NOT INITIAL.
  CHECK wa_zsdt0001us-id_branch IS NOT INITIAL.
  CHECK wa_zsdt0001us-nr_safra IS NOT INITIAL.
  CHECK wa_zsdt0001us-tp_carga IS NOT INITIAL.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
      ID 'WERKS' FIELD  wa_zsdt0001us-id_branch
      ID 'ACTVT' FIELD '03'.    "Alteração

  psafra  = wa_zsdt0001us-nr_safra.
  pempre  = wa_zsdt0001us-id_bukrs.
  pfilia  = wa_zsdt0001us-id_branch.
  ptipca  = wa_zsdt0001us-tp_carga.

  p_ok = abap_true.

ENDFORM.

INCLUDE zmmr126_status_9005.

INCLUDE zmmr126_status_9006.

INCLUDE zmmr126_status_9011.
