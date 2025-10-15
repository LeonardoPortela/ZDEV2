*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Tavares                                         &*
*& Data.....: 12/12/2013                                              &*
*& Descrição: Liberação Embarque - Insumos                            &*
*& Transação: ZSDT0079                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP     |Request     |Data       |Descrição                 &*
*&--------------------------------------------------------------------&*
*& Marcos Faneli  |DEVK937404 |16.05.2014 |                           &*
*& Welgem Barbosa |                                                   &*
*&--------------------------------------------------------------------&*
*& NSEGATIN       |DEVK9A2HNK |07.05.2025 |Ajuste de Melhorias Parte 2&*
*&                                        |Chamado: 169500            &*
*&------------------------------------------------------------------- &*
REPORT  zsdr0037.

INCLUDE <cl_alv_control>.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo.
*----------------------------------------------------------------------*
* ESTRUTURA
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_vbak,
         vkorg   TYPE vbak-vkorg,
         spart   TYPE vbak-spart,
         vkbur   TYPE vbak-vkbur,
         vkgrp   TYPE vbak-vkgrp,
         auart   TYPE vbak-auart,
         erdat   TYPE vbak-erdat,
         kunnr   TYPE vbak-kunnr,
         vbeln   TYPE vbak-vbeln,
         knumv   TYPE vbak-knumv,
         waerk   TYPE vbak-waerk,
         safra   TYPE zsdt0040-safra,
         cultura TYPE zsdt0040-cultura,
       END OF ty_vbak,

       BEGIN OF ty_vbap,
         vbeln  TYPE vbap-vbeln,
         vbelv  TYPE vbap-vbelv,
         matnr  TYPE vbap-matnr,
         arktx  TYPE vbap-arktx,
         matkl  TYPE vbap-matkl,
         kwmeng TYPE vbap-kwmeng,
         vrkme  TYPE vbap-vrkme,
         werks  TYPE vbap-werks,
         netwr  TYPE vbap-netwr, " 23.07.2025 - RAMON - 183689
         netpr  TYPE vbap-netpr,
         mwsbp  TYPE vbap-mwsbp, " RAMON US 183689 14.07.2025
         waerk  TYPE vbap-waerk,
         posnr  TYPE vbap-posnr,
         meins  TYPE vbap-meins,
         route  TYPE vbap-route,                            "FF #143875
       END OF ty_vbap,

       "FF #143875 - inicio
       BEGIN OF ty_trolz,
         azone TYPE trolz-azone,
         lzone TYPE trolz-lzone,
         route TYPE trolz-route,
       END OF ty_trolz,

       BEGIN OF ty_vbpa,
         vbeln TYPE vbpa-vbeln,
         lifnr TYPE vbpa-lifnr,
         lzone TYPE vbpa-lzone,
       END OF ty_vbpa,
       "FF #143875 - fim

       "FF #173947 - inicio
       BEGIN OF ty_itinerario,
         vbeln TYPE vbap-vbeln,
         posnr TYPE vbap-posnr,
         route TYPE vbap-route,
       END OF ty_itinerario,

       BEGIN OF ty_vbkd_inco,
         vbeln TYPE vbap-vbeln,
         posnr TYPE vbap-posnr,
         inco1 TYPE vbkd-inco1,
       END OF ty_vbkd_inco,
       "FF #173947 - fim

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         wrkst TYPE mara-wrkst,
         mtart TYPE mtart,
       END OF ty_mara,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,

       BEGIN OF ty_vbfa,
         vbelv   TYPE vbfa-vbelv,
         posnv   TYPE vbfa-posnv,
         vbtyp_n TYPE vbfa-vbtyp_n,
         vbtyp_v TYPE vbfa-vbtyp_v,
         vbeln   TYPE vbfa-vbeln,
         posnn   TYPE vbfa-posnn,
         rfmng   TYPE vbfa-rfmng,
         rfwrt   TYPE vbfa-rfwrt,
       END OF ty_vbfa,

       BEGIN OF ty_zsdt0082,
         posnr       TYPE zsdt0082-posnr,
         vbeln       TYPE zsdt0082-vbeln,
         qte_sol     TYPE zsdt0082-qte_sol,
         qte_lib     TYPE zsdt0082-qte_lib,
         nro_sol     TYPE zsdt0082-nro_sol,
         seq         TYPE zsdt0082-seq,
         dt_sol      TYPE zsdt0082-dt_sol,
         usuario_sol TYPE zsdt0082-usuario_sol,
         vkorg       TYPE zsdt0082-vkorg,
         spart       TYPE zsdt0082-spart,
         vkgrp       TYPE zsdt0082-vkgrp,
         vkbur       TYPE zsdt0082-vkbur,
         auart       TYPE zsdt0082-auart,
         status      TYPE zsdt0082-status,
         dt_entrega  TYPE zsdt0082-dt_entrega,
         werks       TYPE zsdt0082-werks,
         nr_rot      TYPE zsdt0082-nr_rot,
         qtd_vinc    TYPE zsdt0131-qtd_vinc,
         tp_oper     TYPE zsded_tpopr,           "<<<------"169500 - NMS------>>>
       END OF ty_zsdt0082,

       BEGIN OF ty_saida,
         checkbox(1),
         kunnr          TYPE vbak-kunnr,
         name1          TYPE kna1-name1,
         vkbur          TYPE vbak-vkbur,
         vbeln          TYPE vbap-vbeln,
         posnr          TYPE vbap-posnr,
         matnr          TYPE vbap-matnr,
         arktx          TYPE vbap-arktx,
         wrkst          TYPE mara-wrkst,
         meins          TYPE vbap-meins,
         kwmeng         TYPE vbap-kwmeng,
         qte_faturado   TYPE vbfa-rfmng,
         qte_sol        TYPE vbap-kwmeng,
         " 03.07.2025 - RAMON - 183689 -->
         netwr          TYPE vbap-netwr,
         mwsbp          TYPE vbap-mwsbp, "<- 29.08.2025
         netpr          TYPE vbap-netwr,
         netpr_p        TYPE p DECIMALS 4,
         mwsbp_u        TYPE vbap-mwsbp,
         vlr_apr        TYPE vbap-netwr,
         vlr_sol        TYPE vbap-netwr,
         kmein_p        TYPE vbap-meins, " kmein do preço
         proc_novo(1),
         " 03.07.2025 - RAMON - 183689 --<
         qte_disponivel TYPE vbap-kwmeng,
         qte_liberar    TYPE vbap-kwmeng,
         observacao(4),
         roteiro(4),
*         MARK,
         vkorg          TYPE vbak-vkorg,
         spart          TYPE vbak-spart,
         vkgrp          TYPE vbak-vkgrp,
         auart          TYPE vbak-auart,
         saldo_f_ov     TYPE vbap-kwmeng,
         saldo_f_sol    TYPE vbap-kwmeng,
         dt_req_ent     TYPE sy-datum,
         werks          TYPE vbap-werks,
         dt_entrega     TYPE zsdt0082-dt_entrega,
         kwert          TYPE konv-kwert,
         waerk          TYPE vbak-waerk,
         mtart          TYPE mtart,
         nr_rot         TYPE zsdt0132-nr_rot,
         color(4)       TYPE c,
         qte_saldo      TYPE vbap-kwmeng,
         safra          TYPE zsdt0040-safra,
         cultura        TYPE zsdt0040-cultura,
         itinerario     TYPE trolz-route,                   "FF #143875
         incoterms      TYPE vbkd-inco1,                    "FF #173947
         itinerario_ov  TYPE vbap-route,                    "FF #173947
         tp_oper        TYPE zsded_tpopr,                   "<<<------"169500 - NMS------>>>
         tp_oper_d      TYPE val_text,                      "<<<------"169500 - NMS------>>>
         t_style        TYPE lvc_t_styl,
       END OF ty_saida,

       BEGIN OF ty_saida2,
         posnr         TYPE zsdt0082-posnr,
         vbeln         TYPE zsdt0082-vbeln,
         dt_sol        TYPE zsdt0082-dt_sol,
         qte_sol       TYPE zsdt0082-qte_sol,
         xsdoor        TYPE zsdt0082-qte_sol,
         xsdo          TYPE zsdt0082-qte_sol,
         qte_lib       TYPE zsdt0082-qte_lib,
         nro_sol       TYPE zsdt0082-nro_sol,
         seq           TYPE zsdt0082-seq,
         vkorg         TYPE zsdt0082-vkorg,
         spart         TYPE zsdt0082-spart,
         vkgrp         TYPE zsdt0082-vkgrp,
         vkbur         TYPE zsdt0082-vkbur,
         auart         TYPE zsdt0082-auart,
         dt_entrega    TYPE zsdt0082-dt_entrega,
         status        TYPE char20,
         alt(1),
         observacao(4),
         roteiro(4),
         nr_rot        TYPE zsdt0082-nr_rot,
         rot_desc      TYPE zsdt0132-rot_desc,
         qtd_vinc      TYPE zsdt0131-qtd_vinc,
         tp_oper       TYPE zsded_tpopr,           "<<<------"169500 - NMS------>>>
         tp_oper_d     TYPE val_text,              "<<<------"169500 - NMS------>>>
       END OF ty_saida2.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_centro,
         bukrs TYPE vbak-vkbur.
TYPES END OF ty_centro.

TYPES: BEGIN OF ty_aux,
         kunnr    TYPE vbak-kunnr,
         vkbur    TYPE vbak-vkbur,
         vbeln    TYPE vbap-vbeln,
         posnr    TYPE vbap-posnr,
         matnr    TYPE vbap-matnr,
         arktx    TYPE vbap-arktx,
         wrkst    TYPE mara-wrkst,
         vkorg    TYPE vbak-vkorg,
         spart    TYPE vbak-spart,
         vkgrp    TYPE vbak-vkgrp,
         tipo(30),
         tdformat TYPE tline-tdformat,
         tdline   TYPE tline-tdline,
       END OF ty_aux.

TYPES: BEGIN OF ty_zsdt,
         vbeln         TYPE zsdt0041-vbeln,
         doc_simulacao TYPE zsdt0090-doc_simulacao,
       END OF ty_zsdt.

TYPES: BEGIN OF ty_zsdt0090,
         vbeln         TYPE zsdt0090-vbeln,
         doc_simulacao TYPE zsdt0090-doc_simulacao,
       END OF ty_zsdt0090.

TYPES: BEGIN OF ty_zsdt0041,
         vbeln         TYPE zsdt0041-vbeln,
         doc_simulacao TYPE zsdt0041-doc_simulacao,
       END OF ty_zsdt0041.
**<<<------"169500 - NMS - INI------>>>
TYPES: BEGIN OF ty_tpopr,
         domvalue_l TYPE domvalue_l,
         ddtext     TYPE val_text,
       END   OF ty_tpopr.
**<<<------"169500 - NMS - FIM------>>>

DATA: t_aux    TYPE TABLE OF ty_aux,
      wa_aux   TYPE ty_aux,
      wg_ucomm TYPE sy-ucomm,
      tg_texto TYPE TABLE OF tline WITH HEADER LINE,
      wg_texto TYPE tline.
*      TG_TEXTO TYPE CATSXT_LONGTEXT_ITAB.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: t_vbak          TYPE TABLE OF ty_vbak,
      t_vbpa          TYPE TABLE OF ty_vbpa,                "FF #143875
      t_trolz         TYPE TABLE OF ty_trolz,               "FF #143875
      t_vbkd_inco     TYPE TABLE OF ty_vbkd_inco,           "FF #173947
      t_itinerario    TYPE TABLE OF ty_itinerario,          "FF #173947
      t_vbap          TYPE TABLE OF ty_vbap,
      t_saldo         TYPE TABLE OF zsd_in_est_saldo_01, " 183689 - RAMON - 14.07.2025 -
      t_vbap_bloq     TYPE TABLE OF ty_vbap,
      t_mara          TYPE TABLE OF ty_mara,
      t_kna1          TYPE TABLE OF ty_kna1,
      t_vbfa          TYPE TABLE OF ty_vbfa,
      t_vbfa_h        TYPE TABLE OF ty_vbfa,
      t_vbfa_h_e      TYPE TABLE OF ty_vbfa,
      t_vbfa_aux      TYPE TABLE OF ty_vbfa,
      t_0082          TYPE TABLE OF ty_zsdt0082,
      lt_0082         TYPE TABLE OF ty_zsdt0082,
      lw_0082         TYPE ty_zsdt0082,
      it_0082         TYPE TABLE OF ty_zsdt0082,
      it_temp         TYPE TABLE OF ty_zsdt0082,
      t_saida         TYPE TABLE OF ty_saida,
      t_saida_ttl     TYPE TABLE OF ty_saida2,
      t_saida2        TYPE TABLE OF ty_saida,
      it_saida2       TYPE TABLE OF ty_saida2,
      it_0116         TYPE TABLE OF zsdt0116,
      it_zsdt0131     TYPE STANDARD TABLE OF zsdt0131,
      it_zsdt0138     TYPE STANDARD TABLE OF zsdt0138,
      it_zsdt0082_aux TYPE STANDARD TABLE OF zsdt0082,
      it_zsdt0041     TYPE TABLE OF ty_zsdt0041,
      it_zsdt0090     TYPE TABLE OF ty_zsdt0090,
      it_zsdt0040     TYPE TABLE OF zsdt0040,
      tg_tpopr        TYPE TABLE OF ty_tpopr,
      it_zsdt         TYPE TABLE OF ty_zsdt.            "<<<------"169500 - NMS------>>>

*----------------------------------------------------------------------*
* WORS AREA
*----------------------------------------------------------------------*
DATA: wa_vbak      TYPE ty_vbak,
      wa_vbak_aux  TYPE ty_vbak,
      wa_vbap      TYPE ty_vbap,
      wa_vbap_bloq TYPE ty_vbap,
      wa_mara      TYPE ty_mara,
      wa_kna1      TYPE ty_kna1,
      wa_vbfa      TYPE ty_vbfa,
      wa_vbfa_h    TYPE ty_vbfa,
      wa_vbfa_h_e  TYPE ty_vbfa,
      wa_0082      TYPE ty_zsdt0082,
      wa_temp      TYPE ty_zsdt0082,
      wa_saida     TYPE ty_saida,
      wa_saida2    TYPE ty_saida2,
      wa_saida_aux TYPE ty_zsdt0082,
      it_centro    TYPE TABLE OF ty_centro WITH HEADER LINE,
      wa_zsdt0041  TYPE ty_zsdt0041,
      wa_zsdt0090  TYPE ty_zsdt0090,
      wa_zsdt0040  TYPE zsdt0040,
      wa_zsdt      TYPE ty_zsdt,
      it_edit      TYPE lvc_t_styl,
      wa_edit      TYPE lvc_s_styl.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE          slis_alv_event,
      events       TYPE          slis_t_event,
      t_print      TYPE          slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE          ty_estrutura,
      v_report     LIKE          sy-repid,
      t_top        TYPE          slis_t_listheader,
      wl_layout    TYPE          slis_layout_alv,
      init,
      wl_name      TYPE thead-tdname,
      wl_id        TYPE thead-tdid,
      var_refresh  TYPE c,
      l_tabix      TYPE sy-tabix,
      l_user_lock  TYPE sy-uname.

DATA w_style TYPE lvc_s_styl.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV POPUP
*----------------------------------------------------------------------*
DATA:
  c_alv_tm    TYPE REF TO cl_alv_grid_toolbar_manager,
  wa_cont     TYPE REF TO cl_gui_custom_container,
  wa_alv      TYPE REF TO cl_gui_alv_grid,
  ty_toolbar  TYPE stb_button,
  it_fcat     TYPE lvc_t_fcat,
  wa_fcat     TYPE lvc_s_fcat,
  wa_variante TYPE disvariant,
  wa_stable   TYPE lvc_s_stbl,
  wa_layout   TYPE lvc_s_layo,
  it_sel_rows TYPE lvc_t_row,
  wa_sel_rows TYPE lvc_s_row.

*----------------------------------------------------------------------*
* ESTRUTURA TEXTO
*----------------------------------------------------------------------*

DATA : it_flines TYPE TABLE OF tline,
       st_flines TYPE tline.

*----------------------------------------------------------------------*
* OBJETOS/CLASSES
*----------------------------------------------------------------------*
DATA: ref1 TYPE REF TO cl_gui_alv_grid,
      ref2 TYPE REF TO cl_gui_alv_grid.

CLASS zcl_events DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor           IMPORTING io_alv_grid   TYPE REF TO cl_gui_alv_grid,
      on_handle             FOR EVENT user_command  OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_toolbar            FOR EVENT toolbar       OF cl_gui_alv_grid IMPORTING e_object e_interactive sender,
      on_but_clk            FOR EVENT button_click  OF cl_gui_alv_grid IMPORTING es_col_id es_row_no,
      handle_hotspot_click  FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id.

ENDCLASS.

CLASS zcl_events IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT c_alv_tm
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.

  METHOD on_toolbar.

* Adiciona Novos Botoes na Grid
    FREE: ty_toolbar.
    DEFINE toobar.
      ty_toolbar-icon      = &1.
      ty_toolbar-function  = &2.
      ty_toolbar-quickinfo = &3.
      ty_toolbar-text      = &4.
      ty_toolbar-butn_type = &5.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    END-OF-DEFINITION.

    toobar:
            ''     ''         ''                        ''          3,
            '@2O@' 'CANCELAR' 'Cancelar Solicitação'    'Cancelar'  0,
            ''     ''         ''                        ''          3.
    CALL METHOD c_alv_tm->reorganize( io_alv_toolbar = e_object ).

  ENDMETHOD.

  METHOD on_handle.

    DATA: werks      TYPE zsdt0082-werks.

    DATA: r_werks TYPE RANGE OF werks_d.

    DATA: it_values  TYPE TABLE OF rgsb4,
          it_0082_c1 TYPE TABLE OF zsdt0082,
          it_0082_c  TYPE TABLE OF zsdt0082.

    DATA: wa_values TYPE rgsb4,
          wa_werks  LIKE LINE OF r_werks.


    FREE: it_sel_rows[], wa_sel_rows.

    CALL METHOD wa_alv->get_selected_rows
      IMPORTING
        et_index_rows = it_sel_rows.

    CHECK NOT it_sel_rows IS INITIAL.
    CHECK lines( it_sel_rows ) EQ 1.

    READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
    READ TABLE it_saida2 INTO DATA(wa_cancelar) INDEX wa_sel_rows-index.



    REFRESH it_values.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = 'MV45AFZZ_WERKS'
        table         = 'VBAP'
        class         = '0000'
        fieldname     = 'WERKS'
      TABLES
        set_values    = it_values
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc IS INITIAL.

      LOOP AT it_values INTO wa_values.
        wa_werks = 'IEQ'.
        wa_werks-low    = wa_values-from.
        IF wa_values-to IS NOT INITIAL.
          wa_werks = 'IBT'.
          wa_werks-high = wa_values-to.
        ENDIF.

        APPEND wa_werks TO r_werks.
      ENDLOOP.
    ENDIF.


    CASE e_ucomm.
      WHEN 'CANCELAR'.

        var_refresh = abap_true.

*-US191034-17.09.2025-#191034-JT-inicio
*---------------------------------------------
*------ check se solicitacao desta OV esta bloqueada/pendente na distribuicao (zsdt0081)
*---------------------------------------------
*        SELECT SINGLE nro_sol, bloqueio
*          INTO @DATA(_zsdt0082)
*          FROM zsdt0082
*         WHERE nro_sol  = @wa_cancelar-nro_sol
*           AND vbeln    = @wa_cancelar-vbeln
*           AND posnr    = @wa_cancelar-posnr
*           AND seq      = '001'.
*
*        IF sy-subrc = 0 AND _zsdt0082-bloqueio = abap_true.
*          MESSAGE s024(sd) WITH 'Solicitacao está Pendente na Distribuicao!!' DISPLAY LIKE 'E'.
*          EXIT.
*        ENDIF.
**-US191034-17.09.2025-#191034-JT-fim
*
*        "FF #145609 - inicio
*        " Chama o método "cancelar_ordem_vendas" diretamente com a instância criada por NEW
*        DATA(lv_msg) = NEW zcl_util_sd( )->cancelar_ordem_vendas(
*          EXPORTING
*            i_nro_sol   = wa_cancelar-nro_sol
*            i_vbeln     = wa_cancelar-vbeln
*            i_posnr     = wa_cancelar-posnr
*        ).
*-US191683-25.09.2025-#191683-JT-inicio
        DATA(lv_mess) = zcl_solicitacao_saida_insumos=>cancelar( iv_nro_sol = wa_cancelar-nro_sol
                                                                 iv_vbeln   = wa_cancelar-vbeln
                                                                 iv_posnr   = wa_cancelar-posnr ).

        IF lv_mess IS NOT INITIAL.
          MESSAGE lv_mess TYPE 'I'.
          RETURN.
        ENDIF.
*-US191683-25.09.2025-#191683-JT-fim

        wa_saida-vbeln = wa_cancelar-vbeln. " 29.08.2025
        wa_saida-posnr = wa_cancelar-posnr. " 29.08.2025

*        SELECT SINGLE werks
*          FROM vbap
*            INTO werks
*              WHERE vbeln EQ wa_cancelar-vbeln
*                AND posnr EQ wa_cancelar-posnr.
*
*        SELECT *
*             FROM zsdt0082
*               INTO TABLE it_0082_c
*                 WHERE nro_sol EQ wa_cancelar-nro_sol
*                   AND seq NE 1.
*
**        IF wa_saida-mtart EQ 'ZFER' AND werks EQ '0175'.
*        IF wa_saida-mtart EQ 'ZFER' AND werks IN r_werks.
*
*          IF line_exists( it_0082_c[ status = 5 ] ).
*            MESSAGE s836(sd) WITH TEXT-005 TEXT-007.
*          ELSE.
*
*            UPDATE zsdt0082 SET status    = 3
*                                user_canc = sy-uname
*                                dt_canc   = sy-datum
*                                  WHERE seq     EQ 1
*                                    AND nro_sol EQ wa_cancelar-nro_sol
*                                    AND vbeln   EQ wa_cancelar-vbeln
*                                    AND posnr   EQ wa_cancelar-posnr.
*
*            UPDATE zsdt0082 SET status    = 4
*                                user_canc = sy-uname
*                                dt_canc   = sy-datum
*                                  WHERE seq     NE 1
*                                    AND nro_sol EQ wa_cancelar-nro_sol
*                                    AND vbeln   EQ wa_cancelar-vbeln
*                                    AND posnr   EQ wa_cancelar-posnr.
*          ENDIF.
*
*        ELSE.
*
*          IF line_exists( it_0082_c[ status = 2  ]  ).
*            MESSAGE s836(sd) WITH TEXT-005 TEXT-006 .
*          ELSEIF line_exists( it_0082_c[ status = 5  ]  ).
*            MESSAGE s836(sd) WITH TEXT-005 TEXT-006 .
*          ELSE.
*
*            UPDATE zsdt0082 SET status    = 3
*                                user_canc = sy-uname
*                                dt_canc   = sy-datum
*                                  WHERE seq     EQ 1
*                                    AND nro_sol EQ wa_cancelar-nro_sol
*                                    AND vbeln   EQ wa_cancelar-vbeln
*                                    AND posnr   EQ wa_cancelar-posnr.
*
*          ENDIF.
*
*        ENDIF.
*
**-CS2020001303 - 16.10.2021 - JT - inicio
*        PERFORM f_envia_carguero USING wa_cancelar-vbeln
*                                       wa_cancelar-posnr.
**-CS2020001303 - 16.10.2021 - JT - fim

        "FF #145609 - fim

        SELECT *
         FROM zsdt0082
          INTO CORRESPONDING FIELDS OF TABLE it_saida2
            WHERE vbeln EQ wa_saida-vbeln
             AND  posnr EQ wa_saida-posnr
             AND  seq   EQ 1.

*-CS2021000218-13.09.2022-#90108-JT-inicio
*-------------------------------
*- verifica se solicitacao esta cancelada
*-------------------------------
        SELECT *
          FROM zsdt0082
          INTO TABLE @DATA(tc_0082)
         WHERE vbeln  = @wa_saida-vbeln
           AND posnr  = @wa_saida-posnr
           AND status = '3'.

        LOOP AT it_saida2 INTO wa_saida2.
          DATA(l_tabix) = sy-tabix.
          READ TABLE tc_0082 INTO DATA(wc_0082) WITH KEY nro_sol = wa_saida2-nro_sol
                                                         vbeln   = wa_saida2-vbeln
                                                         posnr   = wa_saida2-posnr.
          IF sy-subrc = 0.
            DELETE it_saida2 INDEX l_tabix.
          ENDIF.
        ENDLOOP.
*-CS2021000218-13.09.2022-#90108-JT-fim

        SORT it_saida2 BY nro_sol.
        LOOP AT it_saida2 ASSIGNING FIELD-SYMBOL(<saida>).

          wl_name = ''.
          CONCATENATE <saida>-nro_sol <saida>-seq INTO wl_name.
          PERFORM read_text USING 'ROTE' wl_name.
          <saida>-roteiro = wa_saida2-roteiro.

          PERFORM read_text USING 'OBSE' wl_name.
          <saida>-observacao = wa_saida2-observacao.

          CASE <saida>-status.
            WHEN 3.
              MOVE TEXT-003 TO <saida>-status.
            WHEN 2.
              MOVE TEXT-009 TO <saida>-status.
            WHEN 1.
              MOVE TEXT-004 TO <saida>-status.
          ENDCASE.

        ENDLOOP.

        CALL METHOD wa_alv->refresh_table_display.

    ENDCASE.

  ENDMETHOD.

  METHOD on_but_clk.

    DATA: tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab,
          wl_field TYPE lvc_s_col,
          title    TYPE sytitle,
          v_cont   TYPE i.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          is_table    TYPE lvc_s_stbl.

    FREE: tl_texto, tg_texto.
    CLEAR:wl_texto.

    READ TABLE it_saida2 INTO wa_saida2 INDEX es_row_no-row_id.

    CONCATENATE wa_saida2-nro_sol '001' INTO wl_name.

    CASE es_col_id.
      WHEN 'OBSERVACAO'.
        PERFORM read_text USING 'OBSE' wl_name.
        title = 'Observação'.
      WHEN 'ROTEIRO'.
        PERFORM read_text USING 'ROTE' wl_name.
        title = 'Roteiro'.
    ENDCASE.

    LOOP AT tg_texto INTO wg_texto.
      MOVE: wg_texto-tdline TO wl_texto.
      APPEND wl_texto TO tl_texto.
      CLEAR: wl_texto.
    ENDLOOP.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = title
        im_display_mode = abap_true
      CHANGING
        ch_text         = tl_texto.

    FREE: tl_texto, tg_texto.

  ENDMETHOD.

  METHOD handle_hotspot_click.

    DATA: tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab,
          wl_field TYPE lvc_s_col,
          v_cont   TYPE i.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          is_table    TYPE lvc_s_stbl.

    FREE: tl_texto, tg_texto.
    CLEAR:wl_texto.

    READ TABLE it_saida2 INTO wa_saida2 INDEX e_row_id.
    CONCATENATE wa_saida2-nro_sol '001' INTO wl_name.

    CASE e_column_id.
      WHEN 'NR_ROT'.
        PERFORM read_text USING 'ROTE' wl_name.
    ENDCASE.

    IF tg_texto[] IS NOT INITIAL.

      LOOP AT tg_texto INTO wg_texto.
        MOVE: wg_texto-tdline TO wl_texto.
        APPEND wl_texto TO tl_texto.
        CLEAR: wl_texto.
      ENDLOOP.

    ELSE.

      PERFORM read_text_roteiro USING wa_saida2-nr_rot.

      LOOP AT tg_texto INTO wg_texto.
        MOVE: wg_texto-tdline TO wl_texto.
        APPEND wl_texto TO tl_texto.
        CLEAR: wl_texto.
      ENDLOOP.

    ENDIF.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = 'Roteiro'
        im_display_mode = abap_true
      CHANGING
        ch_text         = tl_texto.

    FREE: tl_texto, tg_texto.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK


ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      handle_on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id
                  es_row_no,

      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified
                  et_good_cells,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_on_button_click.

    DATA: tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab,
          wl_field TYPE lvc_s_col,
          v_cont   TYPE i,
          c_x      TYPE c.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          is_table    TYPE lvc_s_stbl.

    REFRESH tl_texto.
    CLEAR:wl_texto.

    IF es_col_id EQ 'OBSERVACAO'.
      READ TABLE t_saida INTO wa_saida INDEX es_row_no-row_id.
      LOOP AT t_aux INTO wa_aux WHERE kunnr EQ wa_saida-kunnr
                                 AND  vkbur EQ wa_saida-vkbur
                                 AND  vbeln EQ wa_saida-vbeln
                                 AND  posnr EQ wa_saida-posnr
                                 AND  matnr EQ wa_saida-matnr
                                 AND  arktx EQ wa_saida-arktx
                                 AND  wrkst EQ wa_saida-wrkst
                                 AND  vkorg EQ wa_saida-vkorg
                                 AND  spart EQ wa_saida-spart
                                 AND  vkgrp EQ wa_saida-vkgrp
                                 AND  tipo  EQ 'OBS'.

        MOVE: wa_aux-tdline   TO wl_texto.

        APPEND wl_texto TO tl_texto.
        CLEAR: wl_texto.
      ENDLOOP.

      IF t_aux[] IS NOT INITIAL.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'OBSE'
            language                = sy-langu
            name                    = ' '
            object                  = 'ZTEXTO'
          TABLES
            lines                   = t_aux
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
      ENDIF.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title        = 'Observação'           "" Título
          im_display_mode = c_x
        CHANGING
          ch_text         = tl_texto.

      IF sy-ucomm EQ 'CX_CONT'.
        IF tl_texto[] IS NOT INITIAL.
          READ TABLE t_saida INTO wa_saida INDEX es_row_no-row_id.
          IF wa_saida-checkbox = ''.
            MOVE icon_display_more TO wa_saida-observacao.
            MODIFY  t_saida FROM wa_saida INDEX es_row_no-row_id TRANSPORTING observacao.

            READ TABLE t_saida INTO wa_saida INDEX es_row_no-row_id.
            DELETE t_aux WHERE kunnr EQ wa_saida-kunnr
                          AND  vkbur EQ wa_saida-vkbur
                          AND  vbeln EQ wa_saida-vbeln
                          AND  posnr EQ wa_saida-posnr
                          AND  matnr EQ wa_saida-matnr
                          AND  arktx EQ wa_saida-arktx
                          AND  wrkst EQ wa_saida-wrkst
                          AND  vkorg EQ wa_saida-vkorg
                          AND  spart EQ wa_saida-spart
                          AND  vkgrp EQ wa_saida-vkgrp
                          AND  tipo  EQ 'OBS'.

            LOOP AT tl_texto INTO wl_texto.
              MOVE: wa_saida-kunnr    TO wa_aux-kunnr,
                    wa_saida-vkbur    TO wa_aux-vkbur,
                    wa_saida-vbeln    TO wa_aux-vbeln,
                    wa_saida-posnr    TO wa_aux-posnr,
                    wa_saida-matnr    TO wa_aux-matnr,
                    wa_saida-arktx    TO wa_aux-arktx,
                    wa_saida-wrkst    TO wa_aux-wrkst,
                    wa_saida-vkorg    TO wa_aux-vkorg,
                    wa_saida-spart    TO wa_aux-spart,
                    wa_saida-vkgrp    TO wa_aux-vkgrp,
                    '*'               TO wa_aux-tdformat,
                    wl_texto          TO wa_aux-tdline,
                    'OBS'             TO wa_aux-tipo.


              APPEND wa_aux TO t_aux.
              CLEAR:wa_aux.
            ENDLOOP.
          ELSE.
            "MF
            v_cont = 1.
            LOOP AT t_saida INTO wa_saida WHERE checkbox EQ abap_true.
              MOVE icon_display_more TO wa_saida-observacao.
              MODIFY  t_saida FROM wa_saida INDEX es_row_no-row_id TRANSPORTING observacao.

              READ TABLE t_saida INTO wa_saida INDEX es_row_no-row_id.
              DELETE t_aux WHERE kunnr EQ wa_saida-kunnr
                            AND  vkbur EQ wa_saida-vkbur
                            AND  vbeln EQ wa_saida-vbeln
                            AND  posnr EQ wa_saida-posnr
                            AND  matnr EQ wa_saida-matnr
                            AND  arktx EQ wa_saida-arktx
                            AND  wrkst EQ wa_saida-wrkst
                            AND  vkorg EQ wa_saida-vkorg
                            AND  spart EQ wa_saida-spart
                            AND  vkgrp EQ wa_saida-vkgrp
                            AND  tipo  EQ 'OBS'.

              LOOP AT tl_texto INTO wl_texto.
                MOVE: wa_saida-kunnr    TO wa_aux-kunnr,
                      wa_saida-vkbur    TO wa_aux-vkbur,
                      wa_saida-vbeln    TO wa_aux-vbeln,
                      wa_saida-posnr    TO wa_aux-posnr,
                      wa_saida-matnr    TO wa_aux-matnr,
                      wa_saida-arktx    TO wa_aux-arktx,
                      wa_saida-wrkst    TO wa_aux-wrkst,
                      wa_saida-vkorg    TO wa_aux-vkorg,
                      wa_saida-spart    TO wa_aux-spart,
                      wa_saida-vkgrp    TO wa_aux-vkgrp,
                      '*'               TO wa_aux-tdformat,
                      wl_texto          TO wa_aux-tdline,
                      'OBS'             TO wa_aux-tipo.

                APPEND wa_aux TO t_aux.
                CLEAR:wa_aux.

              ENDLOOP.
            ENDLOOP.
          ENDIF.

        ELSE.
          MOVE icon_enter_more TO wa_saida-observacao.
          MODIFY  t_saida FROM wa_saida INDEX es_row_no-row_id TRANSPORTING observacao.

        ENDIF.
      ENDIF.

    ELSEIF es_col_id EQ 'ROTEIRO'.
      READ TABLE t_saida INTO wa_saida INDEX es_row_no-row_id.

*      LOOP AT LT_0082 INTO WA_0082
*         WHERE VBELN EQ WA_SAIDA-VBELN
*           AND POSNR EQ WA_SAIDA-POSNR
*           AND QTE_SOL IS NOT INITIAL
*           AND STATUS NE 3.
*
*        CONCATENATE WA_0082-NRO_SOL WA_0082-SEQ INTO WL_NAME.
*        PERFORM READ_TEXT USING 'ROTE' WL_NAME.
*
*      ENDLOOP.

      LOOP AT tg_texto INTO wg_texto.

        MOVE: wg_texto-tdline TO wl_texto.
        APPEND wl_texto TO tl_texto.
        CLEAR: wl_texto.
      ENDLOOP.

      LOOP AT t_aux INTO wa_aux WHERE kunnr EQ wa_saida-kunnr
                                 AND  vkbur EQ wa_saida-vkbur
                                 AND  vbeln EQ wa_saida-vbeln
                                 AND  posnr EQ wa_saida-posnr
                                 AND  matnr EQ wa_saida-matnr
                                 AND  arktx EQ wa_saida-arktx
                                 AND  wrkst EQ wa_saida-wrkst
                                 AND  vkorg EQ wa_saida-vkorg
                                 AND  spart EQ wa_saida-spart
                                 AND  vkgrp EQ wa_saida-vkgrp
                                 AND  tipo  EQ 'ROTEIRO'.

        MOVE: wa_aux-tdline   TO wl_texto.

        APPEND wl_texto TO tl_texto.
        CLEAR: wl_texto.

      ENDLOOP.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = 'Roteiro'              "" Título
        CHANGING
          ch_text  = tl_texto.

      IF sy-ucomm EQ 'CX_CONT'.
        IF tl_texto[] IS NOT INITIAL.
          IF wa_saida-checkbox = ''.
            MOVE icon_display_more TO wa_saida-roteiro.
            MODIFY  t_saida FROM wa_saida INDEX es_row_no-row_id TRANSPORTING roteiro.

            READ TABLE t_saida INTO wa_saida INDEX es_row_no-row_id.
            DELETE t_aux WHERE kunnr EQ wa_saida-kunnr
                          AND  vkbur EQ wa_saida-vkbur
                          AND  vbeln EQ wa_saida-vbeln
                          AND  posnr EQ wa_saida-posnr
                          AND  matnr EQ wa_saida-matnr
                          AND  arktx EQ wa_saida-arktx
                          AND  wrkst EQ wa_saida-wrkst
                          AND  vkorg EQ wa_saida-vkorg
                          AND  spart EQ wa_saida-spart
                          AND  vkgrp EQ wa_saida-vkgrp
                          AND  tipo  EQ 'ROTEIRO'.

            LOOP AT tl_texto INTO wl_texto.
              MOVE: wa_saida-kunnr    TO wa_aux-kunnr,
                    wa_saida-vkbur    TO wa_aux-vkbur,
                    wa_saida-vbeln    TO wa_aux-vbeln,
                    wa_saida-posnr    TO wa_aux-posnr,
                    wa_saida-matnr    TO wa_aux-matnr,
                    wa_saida-arktx    TO wa_aux-arktx,
                    wa_saida-wrkst    TO wa_aux-wrkst,
                    wa_saida-vkorg    TO wa_aux-vkorg,
                    wa_saida-spart    TO wa_aux-spart,
                    wa_saida-vkgrp    TO wa_aux-vkgrp,
                    '*'               TO wa_aux-tdformat,
                    wl_texto          TO wa_aux-tdline,
                    'ROTEIRO'         TO wa_aux-tipo.

              APPEND wa_aux TO t_aux.
              CLEAR:wa_aux.

            ENDLOOP.

          ELSE.
            LOOP AT t_saida INTO wa_saida WHERE checkbox EQ abap_true.
              MOVE icon_display_more TO wa_saida-roteiro.
              MODIFY  t_saida FROM wa_saida INDEX es_row_no-row_id TRANSPORTING roteiro.

              READ TABLE t_saida INTO wa_saida INDEX es_row_no-row_id.
              DELETE t_aux WHERE kunnr EQ wa_saida-kunnr
                            AND  vkbur EQ wa_saida-vkbur
                            AND  vbeln EQ wa_saida-vbeln
                            AND  posnr EQ wa_saida-posnr
                            AND  matnr EQ wa_saida-matnr
                            AND  arktx EQ wa_saida-arktx
                            AND  wrkst EQ wa_saida-wrkst
                            AND  vkorg EQ wa_saida-vkorg
                            AND  spart EQ wa_saida-spart
                            AND  vkgrp EQ wa_saida-vkgrp
                            AND  tipo  EQ 'ROTEIRO'.

              LOOP AT tl_texto INTO wl_texto.
                MOVE: wa_saida-kunnr    TO wa_aux-kunnr,
                      wa_saida-vkbur    TO wa_aux-vkbur,
                      wa_saida-vbeln    TO wa_aux-vbeln,
                      wa_saida-posnr    TO wa_aux-posnr,
                      wa_saida-matnr    TO wa_aux-matnr,
                      wa_saida-arktx    TO wa_aux-arktx,
                      wa_saida-wrkst    TO wa_aux-wrkst,
                      wa_saida-vkorg    TO wa_aux-vkorg,
                      wa_saida-spart    TO wa_aux-spart,
                      wa_saida-vkgrp    TO wa_aux-vkgrp,
                      '*'               TO wa_aux-tdformat,
                      wl_texto          TO wa_aux-tdline,
                      'ROTEIRO'         TO wa_aux-tipo.

                APPEND wa_aux TO t_aux.
                CLEAR:wa_aux.

              ENDLOOP.
            ENDLOOP.
          ENDIF.

        ELSE.
          MOVE icon_enter_more TO wa_saida-roteiro.
          MODIFY t_saida FROM wa_saida INDEX es_row_no-row_id TRANSPORTING roteiro.

        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        es_sel_hide = ls_sel_hide
        e_grid      = ref1.

    CALL METHOD ref1->refresh_table_display
      EXPORTING
        is_stable = is_table.

    CALL METHOD cl_gui_cfw=>dispatch.
    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD.                    "HANDLE_ON_BUTTON_CLICK.

  METHOD on_data_changed.

    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          lv_sol     TYPE vbap-kwmeng,
          lv_roteiro TYPE zsdt0132-nr_rot.

    DATA: e_route  TYPE trolz-route,
          e_return TYPE bapiret2.

    FIELD-SYMBOLS: <fs_saida> TYPE ty_saida,
                   <fs_field> TYPE any.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE t_saida ASSIGNING <fs_saida> INDEX ls_good-row_id.

      ASSIGN COMPONENT ls_good-fieldname OF STRUCTURE <fs_saida> TO <fs_field>.

      CASE ls_good-fieldname.

        WHEN 'NR_ROT'.                                      "FF #143875

          MOVE ls_good-value TO lv_roteiro.

          " 03.09.2025 -->

          DATA(roteiro_invalido) = abap_false.

          SELECT SINGLE zsdt0132~nr_rot, zsdt0132~rot_desc, zsdt0132~city1, zsdt0132~uf,
                 tzone~zone1, tzone~zlatitude, tzone~zlongitude, tzone~z_url_localizacao
            INTO @DATA(lwa_lat_lon)
            FROM zsdt0132
            INNER JOIN tzone ON tzone~land1 = 'BR'
                            AND tzone~zone1 = zsdt0132~lzone
            WHERE zsdt0132~status  EQ 'A'
              AND zsdt0132~nr_rot  EQ @lv_roteiro.

          IF sy-subrc NE 0.

            MESSAGE 'Roteiro não encontrado!' TYPE 'I' DISPLAY LIKE 'E'.

            CLEAR ls_good-value.

            roteiro_invalido = abap_true.

          ELSE.

            CALL METHOD zcl_manutencao_insumos=>chk_latitude_longitude
              EXPORTING
                i_latitude  = lwa_lat_lon-zlatitude
                i_longitude = lwa_lat_lon-zlongitude
                i_origem    = abap_true
              IMPORTING
                e_msg       = DATA(e_msg).

            IF e_msg IS NOT INITIAL.
              roteiro_invalido = abap_true.
              MESSAGE |Problema no Roteiro Ponto Coleta: { lv_value }! Erro: { e_msg } | TYPE 'I' DISPLAY LIKE 'E'.
            ENDIF.

            IF roteiro_invalido = abap_true.
              CLEAR ls_good-value.
            ENDIF.

          ENDIF.

          IF roteiro_invalido = abap_false.

            " 03.09.2025 --<
            CALL FUNCTION 'ZSD_BUSCA_ITINERARIO'
              EXPORTING
                i_roteiro = lv_roteiro
                i_vbeln   = <fs_saida>-vbeln
              IMPORTING
                e_route   = e_route
                e_return  = e_return.

            IF e_route IS NOT INITIAL.
              <fs_saida>-itinerario = e_route.

            ELSEIF e_return-type = 'E'.

              CLEAR: <fs_saida>-itinerario.

              MESSAGE e_return-message TYPE 'I'.

            ENDIF.

          ENDIF.

        WHEN 'QTE_LIBERAR'.

          MOVE ls_good-value TO lv_sol.

          IF lv_sol LE <fs_saida>-qte_disponivel.

            <fs_field> = ls_good-value.
            <fs_saida>-checkbox = abap_true.

            MOVE ls_good-value TO lv_value.
            CONDENSE lv_value NO-GAPS.

            IF <fs_saida>-qte_liberar IS INITIAL.
              <fs_saida>-color = ''.
            ELSE.

              " 03.07.2025 - RAMON - 183689 -->

              IF wa_saida-kmein_p = 'TO' AND wa_saida-proc_novo IS INITIAL.
                " 29.08.2025 -->
                "<fs_saida>-vlr_sol =  ( ( <fs_saida>-qte_sol / 1000 ) + <fs_saida>-qte_liberar ) * <fs_saida>-netpr_p.

                <fs_saida>-vlr_sol =  ( ( <fs_saida>-netwr + <fs_saida>-mwsbp ) / <fs_saida>-kwmeng ) *
                                        ( ( <fs_saida>-qte_sol / 1000 ) + <fs_saida>-qte_liberar ) .
                " 29.08.2025 --<

              ELSE.
                " 29.08.2025 -->
                <fs_saida>-vlr_sol =  ( ( <fs_saida>-netwr + <fs_saida>-mwsbp ) / <fs_saida>-kwmeng ) *
                                        ( <fs_saida>-qte_sol + <fs_saida>-qte_liberar ) .
                "<fs_saida>-vlr_sol =  ( <fs_saida>-qte_sol + <fs_saida>-qte_liberar ) * <fs_saida>-netpr_p.

                " 29.08.2025 --<
              ENDIF.

              " 03.07.2025 - RAMON - 183689 --<

              <fs_saida>-color = 'C500'.

            ENDIF.


          ELSE.
            MESSAGE 'Quantidade informada para é maior que a quantidade disponível.' TYPE 'I'.
            CLEAR: lv_value, ls_good-value, <fs_saida>-vlr_sol.
          ENDIF.

        WHEN 'CHECKBOX'.
          <fs_field> = ls_good-value.
        WHEN 'DT_ENTREGA'.
          <fs_field> = ls_good-value.
**<<<------"169500 - NMS - INI------>>>
        WHEN 'TP_OPER_D'.
          CHECK NOT ls_good-value IS INITIAL.
* Campo Descrição Tipo de Operação (TP_OPER_D).
          READ TABLE tg_tpopr INTO DATA(el_tpopr) WITH KEY ddtext = ls_good-value.
          IF sy-subrc IS INITIAL.
            <fs_field> = ls_good-value.

          ELSE.
            MESSAGE 'Tipo de Operação Inválido. Selecione da pesquisa de ajuda!' TYPE 'I' DISPLAY LIKE 'E'.
            CLEAR ls_good-value.

          ENDIF.
**<<<------"169500 - NMS - FIM------>>>
      ENDCASE.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = ls_good-fieldname
          i_value     = ls_good-value.

      UNASSIGN <fs_saida>.
      UNASSIGN <fs_field>.

    ENDLOOP.
  ENDMETHOD.                    "on_data_chaged

  METHOD on_data_changed_finished.
    CALL METHOD ref1->refresh_table_display
      EXPORTING
        i_soft_refresh = 'X'.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED
  METHOD on_f4.
    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX es_row_no-row_id.
    IF sy-subrc EQ 0.
**<<<------"169500 - NMS - INI------>>>
      CASE e_fieldname.
        WHEN 'NR_ROT'.
**<<<------"169500 - NMS - FIM------>>>
          SET PARAMETER ID 'KUN' FIELD <fs_saida>-kunnr.
**<<<------"169500 - NMS - INI------>>>
        WHEN 'TP_OPER_D'.
* Ajuda der pesquisa Tipo de Operação.
          PERFORM zf_f4_tipo_operacao USING <fs_saida>
                                            e_fieldname.

        WHEN OTHERS.
* Do nothing
      ENDCASE.
**<<<------"169500 - NMS - FIM------>>>
    ENDIF.
  ENDMETHOD.
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION


*       CLASS LCL_EVENT_RECEIVER_POP DEFINITION
CLASS lcl_event_receiver_pop DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      handle_on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified
                  et_good_cells.

ENDCLASS.                    "LCL_EVENT_RECEIVER_POP DEFINITION


*       CLASS LCL_EVENT_RECEIVER_POP IMPLEMENTATION
CLASS lcl_event_receiver_pop IMPLEMENTATION.

  METHOD on_data_changed_finished.
    CALL METHOD ref1->refresh_table_display
      EXPORTING
        i_soft_refresh = abap_true.
  ENDMETHOD.

  METHOD handle_on_button_click.

    DATA: tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab,
          wl_field TYPE lvc_s_col,
          v_cont   TYPE i.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          is_table    TYPE lvc_s_stbl.

    REFRESH tl_texto.
    CLEAR:wl_texto.

    BREAK-POINT.

    IF es_col_id EQ 'OBSERVACAO' OR es_col_id EQ 'ROTEIRO'.

      LOOP AT lt_0082 INTO wa_0082
         WHERE vbeln EQ wa_saida-vbeln
           AND posnr EQ wa_saida-posnr
           AND seq EQ 1.

        CONCATENATE wa_0082-nro_sol wa_0082-seq INTO wl_name.
        PERFORM read_text USING 'ROTE' wl_name.

      ENDLOOP.

      LOOP AT tg_texto INTO wg_texto.

        MOVE: wg_texto-tdline TO wl_texto.
        APPEND wl_texto TO tl_texto.
        CLEAR: wl_texto.
      ENDLOOP.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = 'Roteiro'              "" Título
        CHANGING
          ch_text  = tl_texto.

    ENDIF.

  ENDMETHOD.

  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'XSDO'.

      READ TABLE it_saida2 INTO wa_saida2 INDEX ls_good-row_id.

      IF ls_good-value > wa_saida2-xsdoor.
        lv_value = wa_saida2-xsdo.
        MESSAGE 'Quantidade Solicitada maior que a Disponível.' TYPE 'I'.
      ELSEIF ls_good-value > 0.
        lv_value = ls_good-value.
        CONDENSE lv_value NO-GAPS.

        MOVE: 'X' TO wa_saida2-alt,
              lv_value TO wa_saida2-xsdo.

        MODIFY it_saida2 FROM wa_saida2 INDEX ls_good-row_id TRANSPORTING alt xsdo.
      ELSE.
        lv_value = wa_saida2-xsdoor.
        MESSAGE 'Informe valor maior que 0.' TYPE 'I'.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.                    "on_data_chaged

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION


* TELA DE SELEÇÃO - FORMULÁRIO
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME.
  SELECT-OPTIONS: s_vkorg  FOR wa_vbak-vkorg OBLIGATORY, " Organização de Vendas
                  s_vkbur  FOR wa_vbak-vkbur OBLIGATORY, " Escritório de Vendas
                  s_safra  FOR wa_zsdt0040-safra,
                  s_cult   FOR wa_zsdt0040-cultura,
                  s_spart  FOR wa_vbak-spart,            " Setor de Atividade
                  s_vkgrp  FOR wa_vbak-vkgrp,            " Vendedor
                  s_auart  FOR wa_vbak-auart,            " Tipo de Ordem Venda
                  s_erdat  FOR wa_vbak-erdat OBLIGATORY, " Data Ordem Venda
                  s_matnr  FOR wa_vbap-matnr,
                  s_ordem  FOR wa_vbak-vbeln.
SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-010.
  PARAMETERS: p_asol RADIOBUTTON GROUP a1, " Autorizar
              p_jsol RADIOBUTTON GROUP a1. " Autorizada
SELECTION-SCREEN: END OF BLOCK b2.

PARAMETERS: p_extcal TYPE c NO-DISPLAY. "Parameter para controle da chamada do programa via submit pelo progrma ZSDR0042 "FF #145609
PARAMETERS: p_rota TYPE z_nr_rot NO-DISPLAY. "// Roda de Origem da ZSDT0081
PARAMETERS: p_dt_en TYPE sydatum NO-DISPLAY. "// Data Entrada de Origem da ZSDT0081
PARAMETERS: p_qte_1 TYPE kwmeng NO-DISPLAY.   "// Qtd Solicitada de Origem da ZSDT0081
PARAMETERS: p_qte_2 TYPE kwmeng NO-DISPLAY.   "// Qtd Saldo liberado de Origem da ZSDT0081
PARAMETERS: p_tpope TYPE zsded_tpopr NO-DISPLAY. "// Tipo de Operacao Origem da ZSDT0081


START-OF-SELECTION.

  PERFORM check_dados.
  PERFORM f_unlock_table.

*&      Form  CHECK_DADOS
FORM check_dados .
  DATA: it_0060           TYPE TABLE OF zsdt0060,
        wa_0060           TYPE zsdt0060,
        centro_block(255),
        linhas            TYPE n.


* VERIFICA SE O USUARIO ESTA COM LIBERAÇÃO PARA REALIZAR O PROCESSO
  IF p_extcal IS INITIAL.

    SELECT *
      FROM zsdt0060
        INTO TABLE it_0060
           WHERE vkbur    IN s_vkbur
             AND programa EQ 'ZSDR016'
             AND usnam    EQ sy-uname.

    "FF #173947 - inicio
    IF s_vkbur[] IS NOT INITIAL.
      SELECT
        FROM tvbur
        FIELDS vkbur
            WHERE vkbur IN @s_vkbur
            INTO TABLE @DATA(lt_vtbur).

      LOOP AT lt_vtbur INTO DATA(ls_vtbur).

        READ TABLE it_0060 TRANSPORTING NO FIELDS WITH KEY vkbur = ls_vtbur-vkbur.
        IF sy-subrc <> 0.
          CONCATENATE centro_block ls_vtbur-vkbur INTO centro_block SEPARATED BY ','.
          MOVE ls_vtbur-vkbur TO it_centro.
          APPEND it_centro.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDIF.

*  IF NOT s_vkbur-high IS INITIAL.
*    WHILE s_vkbur-low NE s_vkbur-high.
*
*      READ TABLE it_0060 TRANSPORTING NO FIELDS WITH KEY vkbur = s_vkbur-low.
*      IF NOT sy-subrc IS INITIAL.
*        CONCATENATE centro_block s_vkbur-low INTO centro_block SEPARATED BY ','.
*        MOVE s_vkbur-low TO it_centro.
*        APPEND it_centro.
*      ENDIF.
*
*      s_vkbur-low = s_vkbur-low + 1.
*
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = s_vkbur-low
*        IMPORTING
*          output = s_vkbur-low.
*
*    ENDWHILE.
*  ELSE.
*
*    LOOP AT s_vkbur.
*
*      READ TABLE it_0060 TRANSPORTING NO FIELDS WITH KEY vkbur = s_vkbur-low.
*      IF NOT sy-subrc IS INITIAL.
*        CONCATENATE centro_block s_vkbur-low INTO centro_block SEPARATED BY ','.
*        MOVE s_vkbur-low TO it_centro.
*        APPEND it_centro.
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDIF.

  "FF #173947 - fim.

  IF NOT centro_block IS INITIAL.
    SHIFT centro_block LEFT DELETING LEADING ','.
    MESSAGE s836(sd) WITH TEXT-001 centro_block.
    EXIT.
  ENDIF.

  PERFORM selecionar_dados.
  " 23.07.2025 - RAMON - 183689 -->
  "PERFORM organizar_dados.
  PERFORM organizar_dados_2.
  " 23.07.2025 - RAMON - 183689 --<
  "FF #145609 - inicio

  IF p_extcal IS NOT INITIAL. "Programa sendo chamado por submit pelo ZSDR0038// Não exibe o ALV. Apenas executa o botão "Solic. Embarque"

    DATA: lv_roteiro TYPE zsdt0132-nr_rot.
    DATA: e_route  TYPE trolz-route,
          e_return TYPE bapiret2.

    lv_roteiro = p_rota.

*    IMPORT _SAIDA-NR_ROT      TO LV_ROTEIRO     FROM MEMORY ID 'memory_roteiro'. "Export feito no programa ZSDR0038
    IF lv_roteiro IS NOT INITIAL.
      LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

        CALL FUNCTION 'ZSD_BUSCA_ITINERARIO'
          EXPORTING
            i_roteiro = lv_roteiro
            i_vbeln   = <fs_saida>-vbeln
          IMPORTING
            e_route   = e_route
            e_return  = e_return.

        IF e_route IS NOT INITIAL.
          <fs_saida>-nr_rot = lv_roteiro.
          <fs_saida>-itinerario = e_route.
          <fs_saida>-checkbox = 'X'.

        ELSEIF e_return-type = 'E'.
          MESSAGE e_return-message TYPE 'I'.
        ENDIF.

      ENDLOOP.
    ENDIF.

    DATA lt_branco TYPE kkblo_selfield.
    PERFORM xuser_command USING '&SOL_EMBAR'
                                lt_branco.

  ELSE.

    PERFORM iniciar_variaveis.
    PERFORM imprimir_dados.


  ENDIF.
  "FF #145609 - fim



ENDFORM.

*-IR037399  - jtassoni - 18.09.2020 - inicio
*********************************************************************************
* lock table
*********************************************************************************
FORM f_lock_table.

  DATA: l_flg       TYPE c.

  FREE: l_flg, l_user_lock.

  LOOP AT t_saida INTO wa_saida.
    l_tabix = sy-tabix.

    CALL FUNCTION 'ENQUEUE_EZSDT0082'
      EXPORTING
        mode_zsdt0082  = 'E'
        mandt          = sy-mandt
        vbeln          = wa_saida-vbeln
        posnr          = wa_saida-posnr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      l_user_lock = sy-msgv1.

      FREE: it_edit.
      wa_edit-fieldname = 'CHECKBOX'.
      wa_edit-style     = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wa_edit TO it_edit.
      wa_edit-fieldname = 'DT_ENTREGA'.
      wa_edit-style     = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wa_edit TO it_edit.
      wa_edit-fieldname = 'NR_ROT'.
      wa_edit-style     = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wa_edit TO it_edit.
      wa_edit-fieldname = 'QTE_LIBERAR'.
      wa_edit-style     = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wa_edit TO it_edit.
**<<<------"169500 - NMS - INI------>>>
      wa_edit-fieldname = 'TP_OPER_D'.
      wa_edit-style     = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wa_edit TO it_edit.
**<<<------"169500 - NMS - FIM------>>>
      wa_saida-t_style[] = it_edit[].
      MODIFY t_saida FROM wa_saida INDEX l_tabix.

*      IF l_flg IS INITIAL.
*        MESSAGE i000(fb) WITH 'Registro(s) bloqueado(s) pelo usuário :'
*                              sy-msgv1.
*        l_flg = abap_true.
*      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*-IR037399  - jtassoni - 18.09.2020 - fim

*********************************************************************************
* unlock table
*********************************************************************************
FORM f_unlock_table.

  LOOP AT t_saida INTO wa_saida.
    l_tabix = sy-tabix.

    CALL FUNCTION 'DEQUEUE_EZSDT0082'
      EXPORTING
        mode_zsdt0082  = 'E'
        mandt          = sy-mandt
        vbeln          = wa_saida-vbeln
        posnr          = wa_saida-posnr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
  ENDLOOP.

ENDFORM.
*-IR037399  - jtassoni - 18.09.2020 - fim

*&      Form  SELECIONAR_DADOS
FORM  selecionar_dados .

  FREE: t_saida, t_vbap, t_vbap, t_kna1, t_mara, t_vbfa, t_0082, t_vbfa_aux, t_aux, it_0116.

  SELECT *
    FROM zsdt0116
  INTO TABLE it_0116
    WHERE status NE 'X'
      AND vbeln IN s_ordem.

  DELETE it_0116 WHERE status_workflow EQ 'L'.
  DELETE it_0116 WHERE status_workflow EQ 'R'.

  CHECK NOT it_0116 IS INITIAL.

  SELECT a~vkorg a~spart a~vkbur a~vkgrp a~auart a~erdat a~kunnr a~vbeln a~knumv a~waerk
    FROM vbak AS a
    INNER JOIN vbep AS b ON a~vbeln EQ b~vbeln
      INTO TABLE t_vbak
    FOR ALL ENTRIES IN it_0116
      WHERE a~vbeln EQ it_0116-vbeln
        AND a~vkorg IN s_vkorg
        AND a~spart IN s_spart
        AND a~vkbur IN s_vkbur
        AND a~vkgrp IN s_vkgrp
        AND a~auart IN s_auart
        AND a~erdat IN s_erdat
        AND b~lifsp NE '12'.

  IF sy-subrc IS INITIAL.

    SELECT vbeln doc_simulacao FROM zsdt0090 INTO CORRESPONDING FIELDS OF TABLE it_zsdt
      FOR ALL ENTRIES IN t_vbak
     WHERE vbeln EQ  t_vbak-vbeln.

*    MOVE-CORRESPONDING IT_ZSDT0090 TO IT_ZSDT.

    SELECT vbeln doc_simulacao FROM zsdt0041 APPENDING CORRESPONDING FIELDS OF TABLE it_zsdt
      FOR ALL ENTRIES IN t_vbak
     WHERE vbeln EQ  t_vbak-vbeln.

*    MOVE-CORRESPONDING IT_ZSDT0041 TO IT_ZSDT.

    SELECT  * FROM zsdt0040 INTO TABLE it_zsdt0040
      FOR ALL ENTRIES IN it_zsdt
       WHERE doc_simulacao EQ it_zsdt-doc_simulacao
        AND  safra         IN s_safra
        AND  cultura       IN s_cult.


    SELECT a~vbeln a~vbelv a~matnr a~arktx a~matkl a~kwmeng a~vrkme a~werks a~netwr a~netpr a~mwsbp a~waerk a~posnr a~meins a~route
      FROM vbap AS a
        INNER JOIN vbep AS b
        ON b~vbeln EQ a~vbeln "USER STORY 131069 - MMSILVA - 15.01.2025
        AND b~posnr EQ a~posnr"USER STORY 131069 - MMSILVA - 15.01.2025
        INTO TABLE t_vbap
        FOR ALL ENTRIES IN t_vbak
          WHERE a~vbeln EQ t_vbak-vbeln
            AND a~matnr IN s_matnr
            AND b~lifsp <> '12'. "USER STORY 131069 - MMSILVA - 15.01.2025

    IF sy-subrc IS INITIAL.

      "FF #143875 - inicio.

      SELECT v~vbeln, v~lifnr, v~lzone
      FROM vbpa AS v
      INTO TABLE @t_vbpa
      FOR ALL ENTRIES IN @t_vbap
      WHERE vbeln = @t_vbap-vbeln
        AND parvw = 'PC'.

      IF sy-subrc = 0.

        SELECT azone, lzone, route
        FROM trolz FOR ALL ENTRIES IN @t_vbpa
        WHERE azone = @t_vbpa-lzone
          AND route <> @abap_off   "*-US190671-15.09.2025-#190671-JT
        INTO TABLE @t_trolz.

      ENDIF.

      "FF #143875 - fim

      "FF #173947 - inicio
      "Incoterms
      SELECT vbeln, posnr, inco1
        INTO TABLE @t_vbkd_inco
        FROM vbkd
        FOR ALL ENTRIES IN @t_vbap
        WHERE vbeln = @t_vbap-vbeln.

      "Itinerário
      SELECT vbeln, posnr, route
        INTO TABLE @t_itinerario
        FROM vbap
        FOR ALL ENTRIES IN @t_vbap
        WHERE vbeln = @t_vbap-vbeln
          AND posnr = @t_vbap-posnr.
      "FF #173947 - fim

      SELECT matnr wrkst mtart
        FROM mara
          INTO TABLE t_mara
          FOR ALL ENTRIES IN t_vbap
            WHERE matnr EQ t_vbap-matnr.

      SELECT vbelv posnv vbtyp_n vbtyp_v vbeln posnn rfmng rfwrt
        FROM vbfa
          INTO TABLE t_vbfa
          FOR ALL ENTRIES IN t_vbap
            WHERE vbelv EQ t_vbap-vbeln
              AND posnv EQ t_vbap-posnr
        AND   vbtyp_n   IN  ('J', 'H', 'C', 'L' )
        AND   vbtyp_v   EQ  'C'.

*              AND (    VBTYP_N EQ 'M'
*                    OR VBTYP_N EQ 'O' )
*              AND (    VBTYP_V EQ 'C'
*                    OR VBTYP_V EQ 'H' ).

*      T_VBFA_AUX[] = T_VBFA[].
*      DELETE T_VBFA_AUX WHERE VBTYP_N NE 'O'
*                          AND VBTYP_V NE 'H'.

*      IF T_VBFA_AUX IS NOT INITIAL.
*        SELECT VBELV POSNV VBTYP_N VBTYP_V VBELN POSNN RFMNG
*          FROM VBFA
*            INTO TABLE T_VBFA
*            FOR ALL ENTRIES IN T_VBFA_AUX
*              WHERE VBELV EQ T_VBFA_AUX-VBELN
*                AND POSNV EQ T_VBFA_AUX-POSNN
*                AND VBTYP_N EQ 'O'
*                AND VBTYP_V EQ 'H'.

*      ENDIF.


      IF t_vbfa IS NOT INITIAL.
        SELECT vbelv posnv vbtyp_n vbtyp_v vbeln posnn rfmng rfwrt
          FROM vbfa
            INTO TABLE t_vbfa_h
            FOR ALL ENTRIES IN t_vbfa
              WHERE vbelv EQ t_vbfa-vbeln
                AND posnv EQ t_vbfa-posnn
                AND vbtyp_n EQ 'O'
                AND vbtyp_v EQ 'H'.

        IF NOT t_vbfa_h IS INITIAL.

          SELECT vbelv posnv vbtyp_n vbtyp_v vbeln posnn rfmng rfwrt
                   FROM vbfa
                     INTO TABLE t_vbfa_h_e
                     FOR ALL ENTRIES IN t_vbfa_h
                       WHERE vbelv EQ t_vbfa_h-vbeln
                         AND posnv EQ t_vbfa_h-posnn
                         AND vbtyp_n EQ 'S'
                         AND vbtyp_v EQ 'O'.
        ENDIF.

      ENDIF.

      SELECT posnr vbeln qte_sol qte_lib nro_sol seq dt_sol usuario_sol vkorg spart vkgrp vkbur auart status dt_entrega werks nr_rot tp_oper
        FROM zsdt0082
          INTO CORRESPONDING FIELDS OF TABLE t_0082
          FOR ALL ENTRIES IN t_vbap
          WHERE posnr  EQ t_vbap-posnr
            AND vbeln  EQ t_vbap-vbeln
            " 23.07.2025 - RAMON - 183689 -->
            AND status IN ('1','2').
      " 23.07.2025 - RAMON - 183689 --<

      SELECT *
        FROM zsdt0131
        INTO TABLE it_zsdt0131
        FOR ALL ENTRIES IN t_vbap
        WHERE vbeln  EQ t_vbap-vbeln
          AND posnr  EQ t_vbap-posnr
          AND status NE 'X'.

      SELECT *
        FROM zsdt0082
        INTO TABLE it_zsdt0082_aux
        FOR ALL ENTRIES IN t_vbap
        WHERE vbeln  EQ t_vbap-vbeln
          AND posnr  EQ t_vbap-posnr
          AND status EQ 5
          AND EXISTS ( SELECT * FROM zsdt0140 WHERE nro_sol EQ zsdt0082~nro_sol
                                                AND seq     EQ zsdt0082~seq
                                                AND status  NE 'X' ).

      SELECT *
        FROM zsdt0138
        INNER JOIN zsdt0082 ON zsdt0138~nro_sol = zsdt0082~nro_sol AND zsdt0138~seq = zsdt0082~seq
        INTO CORRESPONDING FIELDS OF TABLE it_zsdt0138
        FOR ALL ENTRIES IN t_vbap
        WHERE zsdt0082~vbeln  EQ t_vbap-vbeln
          AND zsdt0082~posnr  EQ t_vbap-posnr
          AND zsdt0138~status NE 'X'.

    ENDIF.

    SELECT kunnr name1
      FROM kna1
        INTO TABLE t_kna1
        FOR ALL ENTRIES IN t_vbak
          WHERE kunnr EQ t_vbak-kunnr.
**<<<------"169500 - NMS - INI------>>>
    SELECT domvalue_l ddtext
     FROM dd07t
     INTO TABLE tg_tpopr
    WHERE domname    EQ 'ZSDD_TPOPR'
      AND ddlanguage EQ 'P'.
**<<<------"169500 - NMS - FIM------>>>
  ENDIF.

  " 183689 - RAMON - 14.07.2025 -->
  IF t_vbap IS NOT INITIAL.

    SELECT * FROM zsd_in_est_saldo_01
      INTO TABLE @t_saldo
        FOR ALL ENTRIES IN @t_vbap
          WHERE vbeln  EQ @t_vbap-vbeln.
  ENDIF.

  " 183689 - RAMON - 14.07.2025 --<


ENDFORM.                    " SELECIONAR_DADOS

*&      Form  ORGANIZAR_DADOS
FORM organizar_dados .

  DATA: wl_totfatd     TYPE vbfa-rfmng,
        wl_tot_qte_sol TYPE zsdt0082-qte_sol,
        wl_tot_qte_lib TYPE zsdt0082-qte_lib,
        v_kwert        TYPE konv-kwert.

  SORT: t_vbak BY vbeln,
        t_kna1 BY kunnr,
        t_mara BY matnr,
        t_vbfa BY vbeln posnv vbtyp_n vbtyp_v.


  CLEAR t_saida_ttl[].

  lt_0082 = t_0082.
  LOOP AT t_vbap INTO wa_vbap.

    CLEAR: wl_tot_qte_sol, wl_tot_qte_lib, wa_saida.

    READ TABLE t_vbak INTO wa_vbak
      WITH KEY vbeln = wa_vbap-vbeln
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE t_kna1 INTO wa_kna1
        WITH KEY kunnr = wa_vbak-kunnr
                 BINARY SEARCH.

      wa_saida-name1 = wa_kna1-name1.
    ENDIF.

    READ TABLE t_mara INTO wa_mara
      WITH KEY matnr = wa_vbap-matnr
               BINARY SEARCH.

    CLEAR wl_totfatd.

    LOOP AT  t_vbfa INTO wa_vbfa
      WHERE vbelv EQ wa_vbap-vbeln
         AND posnv EQ wa_vbap-posnr.

      IF wa_vbfa-vbtyp_n EQ 'J'.
        ADD wa_vbfa-rfmng TO wl_totfatd.
      ENDIF.

      LOOP AT t_vbfa_h INTO wa_vbfa_h WHERE vbelv EQ wa_vbfa-vbeln AND
                                            posnv EQ wa_vbfa-posnn.
        READ TABLE t_vbfa_h_e TRANSPORTING NO FIELDS WITH KEY vbelv = wa_vbfa_h-vbeln
                                                              posnv = wa_vbfa_h-posnn.
        IF NOT sy-subrc IS INITIAL.
          wa_vbfa_h-rfmng = wa_vbfa_h-rfmng * -1.
          ADD wa_vbfa_h-rfmng TO wl_totfatd.
        ENDIF.
        CLEAR wa_vbfa_h.
      ENDLOOP.

*      LOOP AT T_VBFA INTO WA_VBFA
*        WHERE VBELV EQ WA_VBFA-VBELN
*          AND POSNV EQ WA_VBFA-POSNN
*          AND VBTYP_N EQ 'O'
*          AND VBTYP_V EQ 'H'.
*
*        SUBTRACT WA_VBFA-RFMNG FROM WL_TOTFATD.
**      WL_TOTFATD =  WL_TOTFATD + WA_VBFA-RFMNG.
*      ENDLOOP.
      CLEAR wa_vbfa.
    ENDLOOP.

*    READ TABLE T_VBFA INTO WA_VBFA
*      WITH KEY VBELV   = WA_VBAP-VBELN
*               POSNV   = WA_VBAP-POSNR
*               VBTYP_N = 'M'
*               VBTYP_V = 'C'
*               BINARY SEARCH.
*
*    LOOP AT T_VBFA INTO WA_VBFA
*      WHERE VBELV EQ WA_VBFA-VBELN
*        AND POSNV EQ WA_VBFA-POSNN
*        AND VBTYP_N EQ 'O'
*        AND VBTYP_V EQ 'H'.
*
*      WL_TOTFATD =  WL_TOTFATD + WA_VBFA-RFMNG.
*    ENDLOOP.

    LOOP AT t_0082 INTO wa_0082
      WHERE posnr EQ wa_vbap-posnr
        AND vbeln EQ wa_vbap-vbeln.

      IF wa_0082-seq EQ '1' AND  wa_0082-status EQ 1.
        wl_tot_qte_sol = wl_tot_qte_sol + wa_0082-qte_sol.
      ENDIF.

      IF wa_0082-seq GE '1' AND ( wa_0082-status EQ 1 OR wa_0082-status EQ 2 ).
        wl_tot_qte_lib = wl_tot_qte_lib + wa_0082-qte_lib.
      ENDIF.
**<<<------"169500 - NMS - INI------>>>
      wa_saida-tp_oper = wa_0082-tp_oper.
* Descrição do Tipo de Operação.
      READ TABLE tg_tpopr INTO DATA(el_tpopr) WITH KEY domvalue_l = wa_saida-tp_oper.
      IF sy-subrc IS INITIAL.
        wa_saida-tp_oper_d = el_tpopr-ddtext.

      ELSE.
        CLEAR wa_saida-tp_oper_d.

      ENDIF.
**<<<------"169500 - NMS - FIM------>>>
    ENDLOOP.

*    LOOP AT LT_0082 INTO LW_0082
*      WHERE POSNR EQ WA_VBAP-POSNR
*        AND VBELN EQ WA_VBAP-VBELN.
*      CONCATENATE LW_0082-NRO_SOL LW_0082-SEQ INTO WL_NAME.
*      PERFORM READ_TEXT USING 'ROTE' WL_NAME.
*    ENDLOOP.


    "FF #173947 - inicio
    READ TABLE t_vbkd_inco WITH KEY vbeln = wa_vbap-vbeln
                                    posnr = wa_vbap-posnr INTO DATA(wa_vbkd_inco).

    IF sy-subrc = 0.

      wa_saida-incoterms = wa_vbkd_inco-inco1.

    ELSE.
      READ TABLE t_vbkd_inco WITH KEY vbeln = wa_vbap-vbeln INTO wa_vbkd_inco.

      IF sy-subrc = 0.

        wa_saida-incoterms = wa_vbkd_inco-inco1.

      ELSE.
        CLEAR wa_saida-incoterms.
      ENDIF.
    ENDIF.

    READ TABLE t_itinerario WITH KEY vbeln = wa_vbap-vbeln
                                    posnr = wa_vbap-posnr INTO DATA(wa_itinerario).
    IF sy-subrc = 0.

      wa_saida-itinerario_ov = wa_itinerario-route.

    ELSE.
      CLEAR wa_saida-itinerario_ov.
    ENDIF.
    "FF #173947 - fim

    wa_saida-auart          = wa_vbak-auart.
    wa_saida-kunnr          = wa_vbak-kunnr.
    wa_saida-vkbur          = wa_vbak-vkbur.
    wa_saida-vbeln          = wa_vbap-vbeln.
    wa_saida-posnr          = wa_vbap-posnr.
    wa_saida-werks          = wa_vbap-werks.
    wa_saida-matnr          = wa_vbap-matnr.
    wa_saida-arktx          = wa_vbap-arktx.
    wa_saida-wrkst          = wa_mara-wrkst.
    wa_saida-meins          = wa_vbap-meins.
    wa_saida-kwmeng         = wa_vbap-kwmeng. "Qte. Ordem
    wa_saida-mtart          = wa_mara-mtart.
    wa_saida-waerk          = wa_vbak-waerk.

    READ TABLE it_zsdt INTO wa_zsdt WITH KEY vbeln =  wa_vbak-vbeln.
    IF sy-subrc = 0.
      READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt-doc_simulacao.
      IF sy-subrc = 0.
        wa_saida-safra          = wa_zsdt0040-safra.
        wa_saida-cultura        = wa_zsdt0040-cultura.
      ENDIF.
    ENDIF.


    SELECT SINGLE kwert
      FROM konv
      INTO v_kwert
      WHERE knumv EQ wa_vbak-knumv
      AND kposn EQ wa_vbap-posnr
      AND kschl EQ 'PR00'.

    IF sy-subrc EQ 0.
      wa_saida-kwert        = v_kwert.
    ENDIF.

    wa_saida-qte_faturado   = wl_totfatd.                               "Qte. Faturado
    wa_saida-qte_sol        = wl_tot_qte_sol.                           "Qte. Solicitado

    " 03.07.2025 - RAMON - 183689 -->
    wa_saida-netpr = wa_vbap-netpr.
    wa_saida-mwsbp = wa_vbap-mwsbp. "<- 29.08.2025
    wa_saida-vlr_apr  = VALUE #( t_saldo[ vbeln = wa_saida-vbeln ]-vlrtotalautorizado_m DEFAULT '0.00' ).

    IF wa_saida-qte_sol IS NOT INITIAL.
      wa_saida-mwsbp_u = wa_vbap-mwsbp / wa_saida-qte_sol.
      wa_saida-vlr_sol =  wa_saida-qte_sol * ( wa_vbap-netpr + wa_saida-mwsbp_u ).
    ENDIF.
    " 03.07.2025 - RAMON - 183689 --<

    wa_saida-qte_disponivel = wa_vbap-kwmeng   - wa_saida-qte_sol .     "Qte. Disponível
    wa_saida-saldo_f_ov     = wa_vbap-kwmeng   - wa_saida-qte_faturado. "Saldo a Faturar OV
    wa_saida-saldo_f_sol    = wa_saida-qte_sol - wa_saida-qte_faturado. "Saldo a Faturar SOL

    wa_saida-vkorg          = wa_vbak-vkorg.
    wa_saida-spart          = wa_vbak-spart.

    PERFORM get_saldo_lote USING wa_saida-spart
                                 wa_saida-qte_sol
                           CHANGING wa_saida-qte_saldo.

    wa_saida-vkgrp          = wa_vbak-vkgrp.

    wa_saida-dt_req_ent     = ''. "Data Requerida Ent

    wa_saida-observacao     = icon_enter_more.

    IF wa_saida-roteiro IS INITIAL.
      wa_saida-roteiro        = icon_enter_more.
    ENDIF.

    " 23.07.2025 - RAMON - 183689 -->

    CASE abap_true.
      WHEN p_asol.
        IF wa_saida-qte_disponivel IS INITIAL.
          "CONTINUE.
        ENDIF.
      WHEN p_jsol.

      WHEN OTHERS.
    ENDCASE.

    IF  wa_saida-safra IS NOT INITIAL AND  wa_saida-cultura  IS NOT INITIAL.
      APPEND wa_saida TO t_saida.
      CLEAR: wa_saida, wa_saida2, wa_vbak, wa_kna1, wa_mara, wa_0082, wa_vbfa, wa_vbfa_h, wa_vbfa_h_e.
    ENDIF.




*    IF wa_saida-qte_disponivel IS NOT INITIAL.
*
*      IF p_asol EQ abap_true.
*        IF  wa_saida-safra IS NOT INITIAL AND  wa_saida-cultura  IS NOT INITIAL.
*          APPEND wa_saida TO t_saida.
*          CLEAR: wa_saida, wa_vbak, wa_kna1, wa_mara, wa_0082, wa_vbfa, wa_vbfa_h, wa_vbfa_h_e.
*        ENDIF.
*      ENDIF.
*
*    ELSE.
*
*      IF p_jsol EQ abap_true.
*        IF  wa_saida-safra IS NOT INITIAL AND  wa_saida-cultura  IS NOT INITIAL.
*          APPEND wa_saida TO t_saida.
*          CLEAR: wa_saida, wa_vbak, wa_kna1, wa_mara, wa_0082, wa_vbfa, wa_vbfa_h, wa_vbfa_h_e.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.


    " 23.07.2025 - RAMON - 183689 --<

  ENDLOOP.





* DELETE t_saida WHERE saldo_f_sol = 0. "*-CS2025000249-04.07.2025-#181842-JT-inicio

  LOOP AT it_centro.
    DELETE t_saida WHERE vkbur EQ it_centro-bukrs.
  ENDLOOP.

  DATA lv_posnr TYPE posnr_va.

  LOOP AT t_saida INTO wa_saida.

*    " 02.07.2025 - RAMON - 183689 -->
*    IF line_exists( it_0116[ vbeln = wa_saida-vbeln posnr = '000000' ] ).
*      lv_posnr = '000000'.
*      "lv_novo = abap_true.
*    ELSE.
*      lv_posnr = wa_saida-posnr.
*      "lv_novo = abap_false.
*    ENDIF.
*    " 02.07.2025 - RAMON - 183689 --<

*    READ TABLE it_0116 INTO DATA(wl_0116) WITH KEY vbeln = wa_saida-vbeln.
*
*    IF ( NOT sy-subrc IS INITIAL ) OR ( sy-subrc = 0 AND
*                                        wl_0116-status_workflow NE 'A'  AND
*                                        wl_0116-status_workflow IS NOT INITIAL ).
*      DELETE t_saida
*                  WHERE vbeln EQ wa_saida-vbeln AND
*                        posnr EQ wa_saida-posnr.
*    ENDIF.

  ENDLOOP.

  "Teste DEV - inicio
  IF sy-sysid = 'DEV'.
    IF t_saida[] IS INITIAL.
      APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs>).

      <fs>-kunnr = '150'.

    ENDIF.
  ENDIF..
  "Teste DEV - fim


ENDFORM.                    " ORGANIZAR_DADOS
FORM organizar_dados_2 .

  DATA: wl_totfatd     TYPE vbfa-rfmng,
        wl_tot_qte_sol TYPE zsdt0082-qte_sol,
        wl_tot_qte_lib TYPE zsdt0082-qte_lib,
        v_kwert        TYPE konv-kwert.

  SORT: t_vbak BY vbeln,
        t_kna1 BY kunnr,
        t_mara BY matnr,
        t_vbfa BY vbeln posnv vbtyp_n vbtyp_v.


  lt_0082 = t_0082.

  LOOP AT t_vbap INTO wa_vbap.

    "CHECK sy-subrc EQ 0. "<- 29.08.2025

    CLEAR: wl_tot_qte_sol, wl_tot_qte_lib, wa_saida.

    READ TABLE t_vbak INTO wa_vbak
      WITH KEY vbeln = wa_vbap-vbeln
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE t_kna1 INTO wa_kna1
        WITH KEY kunnr = wa_vbak-kunnr
                 BINARY SEARCH.

      wa_saida-name1 = wa_kna1-name1.
    ENDIF.

    READ TABLE t_mara INTO wa_mara
      WITH KEY matnr = wa_vbap-matnr
               BINARY SEARCH.

    CLEAR wl_totfatd.

    LOOP AT  t_vbfa INTO wa_vbfa
      WHERE vbelv EQ wa_vbap-vbeln
         AND posnv EQ wa_vbap-posnr.

      IF wa_vbfa-vbtyp_n EQ 'J'.
        ADD wa_vbfa-rfmng TO wl_totfatd.
      ENDIF.

      LOOP AT t_vbfa_h INTO wa_vbfa_h WHERE vbelv EQ wa_vbfa-vbeln AND
                                            posnv EQ wa_vbfa-posnn.
        READ TABLE t_vbfa_h_e TRANSPORTING NO FIELDS WITH KEY vbelv = wa_vbfa_h-vbeln
                                                              posnv = wa_vbfa_h-posnn.
        IF NOT sy-subrc IS INITIAL.
          wa_vbfa_h-rfmng = wa_vbfa_h-rfmng * -1.
          ADD wa_vbfa_h-rfmng TO wl_totfatd.
        ENDIF.
        CLEAR wa_vbfa_h.
      ENDLOOP.

      CLEAR wa_vbfa.
    ENDLOOP.

*    LOOP AT LT_0082 INTO LW_0082
*      WHERE POSNR EQ WA_VBAP-POSNR
*        AND VBELN EQ WA_VBAP-VBELN.
*      CONCATENATE LW_0082-NRO_SOL LW_0082-SEQ INTO WL_NAME.
*      PERFORM READ_TEXT USING 'ROTE' WL_NAME.
*    ENDLOOP.


    "FF #173947 - inicio
    READ TABLE t_vbkd_inco WITH KEY vbeln = wa_vbap-vbeln
                                    posnr = wa_vbap-posnr INTO DATA(wa_vbkd_inco).

    IF sy-subrc = 0.

      wa_saida-incoterms = wa_vbkd_inco-inco1.

    ELSE.
      READ TABLE t_vbkd_inco WITH KEY vbeln = wa_vbap-vbeln INTO wa_vbkd_inco.

      IF sy-subrc = 0.

        wa_saida-incoterms = wa_vbkd_inco-inco1.

      ELSE.
        CLEAR wa_saida-incoterms.
      ENDIF.
    ENDIF.

    READ TABLE t_itinerario WITH KEY vbeln = wa_vbap-vbeln
                                    posnr = wa_vbap-posnr INTO DATA(wa_itinerario).
    IF sy-subrc = 0.

      wa_saida-itinerario_ov = wa_itinerario-route.

    ELSE.
      CLEAR wa_saida-itinerario_ov.
    ENDIF.
    "FF #173947 - fim

    wa_saida-auart          = wa_vbak-auart.
    wa_saida-kunnr          = wa_vbak-kunnr.
    wa_saida-vkbur          = wa_vbak-vkbur.
    wa_saida-vbeln          = wa_vbap-vbeln.
    wa_saida-posnr          = wa_vbap-posnr.
    wa_saida-werks          = wa_vbap-werks.
    wa_saida-matnr          = wa_vbap-matnr.
    wa_saida-arktx          = wa_vbap-arktx.
    wa_saida-wrkst          = wa_mara-wrkst.
    wa_saida-meins          = wa_vbap-meins.
    wa_saida-kwmeng         = wa_vbap-kwmeng. "Qte. Ordem
    wa_saida-mtart          = wa_mara-mtart.
    wa_saida-waerk          = wa_vbak-waerk.

    READ TABLE it_zsdt INTO wa_zsdt WITH KEY vbeln =  wa_vbak-vbeln.
    IF sy-subrc = 0.
      READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt-doc_simulacao.
      IF sy-subrc = 0.
        wa_saida-safra          = wa_zsdt0040-safra.
        wa_saida-cultura        = wa_zsdt0040-cultura.
      ENDIF.
    ENDIF.


    SELECT SINGLE kwert
      FROM v_konv
      INTO @v_kwert
      WHERE knumv EQ @wa_vbak-knumv
      AND kposn EQ @wa_vbap-posnr
      AND kschl EQ 'ICMI'.

    IF sy-subrc EQ 0.
      wa_saida-kwert        = v_kwert.
    ENDIF.

    " 27.08.2025 - RAMON -->
    SELECT SINGLE kmein
      FROM v_konv
      INTO @wa_saida-kmein_p
      WHERE knumv EQ @wa_vbak-knumv
      AND kposn EQ @wa_vbap-posnr
      AND kschl EQ 'PR00'.
    " 27.08.2025 - RAMON --<

    wa_saida-qte_faturado   = wl_totfatd.                               "Qte. Faturado
    wa_saida-qte_sol        = wl_tot_qte_sol.                           "Qte. Solicitado

    " 29.08.2025 -->
    wa_saida-netwr = wa_vbap-netwr.
    wa_saida-mwsbp = wa_vbap-mwsbp.
    " 29.08.2025 --<

    " 03.07.2025 - RAMON - 183689 -->
    wa_saida-netpr = wa_vbap-netpr.

    wa_saida-netpr_p = ( wa_vbap-netwr + wa_vbap-mwsbp ) / wa_vbap-kwmeng.

    wa_saida-vlr_apr  = VALUE #( t_saldo[ vbeln = wa_saida-vbeln ]-vlrtotalautorizado_m DEFAULT '0.00' ).

    " 27.08.2025 -->
    DATA(lv_exist) = VALUE #( it_0116[ vbeln = wa_saida-vbeln ]-posnr DEFAULT space ).
    IF lv_exist IS INITIAL.
      wa_saida-proc_novo  = abap_true.
    ENDIF.

    " 27.08.2025 --<

    IF wa_saida-qte_sol IS NOT INITIAL.
      wa_saida-mwsbp_u = wa_vbap-mwsbp / wa_saida-qte_sol.
      wa_saida-vlr_sol =  wa_saida-qte_sol * ( wa_vbap-netpr + wa_saida-mwsbp_u ).
    ENDIF.
    " 03.07.2025 - RAMON - 183689 --<

    wa_saida-vkorg          = wa_vbak-vkorg.
    wa_saida-spart          = wa_vbak-spart.

    " 12.09.2025 -->
*    PERFORM get_saldo_lote USING wa_saida-spart
*                                 wa_saida-qte_sol
*                           CHANGING wa_saida-qte_saldo.
    " 12.09.2025 --<

    wa_saida-vkgrp          = wa_vbak-vkgrp.

    wa_saida-dt_req_ent     = ''. "Data Requerida Ent

    wa_saida-observacao     = icon_enter_more.

    IF wa_saida-roteiro IS INITIAL.
      wa_saida-roteiro        = icon_enter_more.
    ENDIF.

    " 23.07.2025 - RAMON - 183689 -->

    " 03.09.2025 -->
    wa_saida-qte_disponivel = wa_vbap-kwmeng.
    wa_saida-saldo_f_ov     = wa_vbap-kwmeng   - wa_saida-qte_faturado. "Saldo a Faturar OV
    wa_saida-saldo_f_sol    = wa_saida-qte_sol - wa_saida-qte_faturado. "Saldo a Faturar SOL
    " 03.09.2025 --<

    LOOP AT t_0082 INTO wa_0082
      WHERE posnr EQ wa_vbap-posnr
        AND vbeln EQ wa_vbap-vbeln.

      " 12.09.2025 - limpando os campos, caso nao entre no if abaixo, estava ficando com valores-->
      CLEAR: wa_saida-qte_sol, wa_saida-vlr_sol, wa_saida-saldo_f_sol,
             wa_saida-qte_saldo, wl_tot_qte_lib.
      " 12.09.2025 --<

      IF wa_0082-seq EQ '1' AND  wa_0082-status EQ 1.

        wa_saida-qte_sol = wa_0082-qte_sol.

        " 03.09.2025 -->
        SUBTRACT wa_saida-qte_sol FROM wa_saida-qte_disponivel.
        " 03.09.2025 --<

        " 23.07.2025 - RAMON - 183689 -->
        IF wa_saida-kmein_p = 'TO' AND wa_saida-proc_novo IS INITIAL.
          wa_saida-vlr_sol = ( wa_saida-qte_sol / 1000 ) * wa_saida-netpr.
        ELSE.
          wa_saida-vlr_sol = wa_saida-qte_sol * wa_saida-netpr.
        ENDIF.
        "wa_saida-vlr_sol = wa_saida-qte_sol * wa_saida-netpr.
        " 23.07.2025 - RAMON - 183689 --<

        wl_tot_qte_sol = wl_tot_qte_sol + wa_0082-qte_sol.

        wa_saida-saldo_f_sol = wa_saida-qte_sol - wa_saida-qte_faturado.

      ENDIF.

      " 12.09.2025 -->
      PERFORM get_saldo_lote USING wa_saida-spart
                                   wa_saida-qte_sol
                             CHANGING wa_saida-qte_saldo.
      " 12.09.2025 --<

      IF wa_0082-seq GE '1' AND ( wa_0082-status EQ 1 OR wa_0082-status EQ 2 ).

        "WA_SAIDA-qt

        wl_tot_qte_lib = wl_tot_qte_lib + wa_0082-qte_lib.
      ENDIF.
**<<<------"169500 - NMS - INI------>>>
      wa_saida-tp_oper = wa_0082-tp_oper.
* Descrição do Tipo de Operação.
      READ TABLE tg_tpopr INTO DATA(el_tpopr) WITH KEY domvalue_l = wa_saida-tp_oper.
      IF sy-subrc IS INITIAL.
        wa_saida-tp_oper_d = el_tpopr-ddtext.

      ELSE.
        CLEAR wa_saida-tp_oper_d.

      ENDIF.
**<<<------"169500 - NMS - FIM------>>>

      IF  wa_saida-safra IS NOT INITIAL AND  wa_saida-cultura  IS NOT INITIAL.

***        " 03.09.2025 -->
***        wa_saida-qte_disponivel = wa_vbap-kwmeng   - wa_saida-qte_sol .     "Qte. Disponível
***        wa_saida-saldo_f_ov     = wa_vbap-kwmeng   - wa_saida-qte_faturado. "Saldo a Faturar OV
***        wa_saida-saldo_f_sol    = wa_saida-qte_sol - wa_saida-qte_faturado. "Saldo a Faturar SOL
***        " 03.09.2025 --<

        " 27.08.2025 - RAMON -->
        IF wa_saida-proc_novo  = abap_true.

          "APPEND wa_saida TO t_saida.

          PERFORM f_collect_alv USING wa_saida.

          wa_saida2-vbeln = wa_saida-vbeln.
          wa_saida2-posnr = wa_saida-posnr.
          wa_saida2-qte_sol =  wa_saida-qte_sol.

          COLLECT wa_saida2 INTO t_saida_ttl.

        ELSE.
          IF wa_0082-status EQ 1.

            "APPEND wa_saida TO t_saida.
            PERFORM f_collect_alv USING wa_saida. " 29.08.2025

            wa_saida2-vbeln = wa_saida-vbeln.
            wa_saida2-posnr = wa_saida-posnr.
            wa_saida2-qte_sol = wa_0082-qte_sol. " WA_SAIDA-QTE_SOL. --- 27.08.2025 - ajuste pos go-live

            COLLECT wa_saida2 INTO t_saida_ttl.

          ENDIF.
        ENDIF.



*        APPEND wa_saida TO t_saida.
*
*        wa_saida2-vbeln = wa_saida-vbeln.
*        wa_saida2-posnr = wa_saida-posnr.
*        wa_saida2-qte_sol = wa_0082-qte_sol. " WA_SAIDA-QTE_SOL. --- 27.08.2025 - ajuste pos go-live
*
*        COLLECT wa_saida2 INTO t_saida_ttl.

        " 27.08.2025 - RAMON --<


      ENDIF.

    ENDLOOP.

    IF NOT line_exists( t_0082[ vbeln = wa_vbap-vbeln posnr = wa_vbap-posnr ] ).

      IF  wa_saida-safra IS NOT INITIAL AND  wa_saida-cultura  IS NOT INITIAL.

        CASE abap_true.
          WHEN p_asol.
            IF wa_saida-qte_disponivel IS INITIAL.
              CONTINUE.
            ENDIF.

            "28.08.2025 -->
            " se for o processo novo, e nao tiver valor aprovado, continua
            IF wa_saida-proc_novo = abap_true AND wa_saida-vlr_apr IS INITIAL.
              CONTINUE.
            ENDIF.
            "28.08.2025 --<

          WHEN p_jsol.
            IF wa_saida-qte_sol IS INITIAL.
              CONTINUE.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

        APPEND wa_saida TO t_saida.

        wa_saida2-vbeln = wa_saida-vbeln.
        wa_saida2-posnr = wa_saida-posnr.
        wa_saida2-qte_sol = wa_saida-qte_sol.

        COLLECT wa_saida2 INTO t_saida_ttl.

      ENDIF.

    ENDIF.

    CLEAR: wa_saida, wa_saida2, wa_vbak, wa_kna1, wa_mara, wa_0082, wa_vbfa, wa_vbfa_h, wa_vbfa_h_e.


*    IF wa_saida-qte_disponivel IS NOT INITIAL.
*
*      IF p_asol EQ abap_true.
*        IF  wa_saida-safra IS NOT INITIAL AND  wa_saida-cultura  IS NOT INITIAL.
*          APPEND wa_saida TO t_saida.
*          CLEAR: wa_saida, wa_vbak, wa_kna1, wa_mara, wa_0082, wa_vbfa, wa_vbfa_h, wa_vbfa_h_e.
*        ENDIF.
*      ENDIF.
*
*    ELSE.
*
*      IF p_jsol EQ abap_true.
*        IF  wa_saida-safra IS NOT INITIAL AND  wa_saida-cultura  IS NOT INITIAL.
*          APPEND wa_saida TO t_saida.
*          CLEAR: wa_saida, wa_vbak, wa_kna1, wa_mara, wa_0082, wa_vbfa, wa_vbfa_h, wa_vbfa_h_e.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.


    " 23.07.2025 - RAMON - 183689 --<

  ENDLOOP.

  LOOP AT t_saida_ttl ASSIGNING FIELD-SYMBOL(<fs_saida_tt>).

    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>)
      WITH KEY vbeln = <fs_saida_tt>-vbeln
               posnr = <fs_saida_tt>-posnr. "<-- 29.08.2025.

    CHECK sy-subrc EQ 0.

    CASE abap_true.
      WHEN p_asol.

        " 27.08.2025 - RAMON - >
        "CHECK <fs_saida>-kwmeng = <fs_saida_tt>-qte_sol.
        CHECK <fs_saida_tt>-qte_sol >= <fs_saida>-kwmeng.
        " 27.08.2025 - RAMON - <

        DELETE t_saida WHERE vbeln = <fs_saida_tt>-vbeln AND posnr = <fs_saida_tt>-posnr. "<-- 29.08.2025

      WHEN p_jsol.

      WHEN OTHERS.
    ENDCASE.


  ENDLOOP.

  SORT t_saida BY vbeln posnr. " 29.08.2025

* DELETE t_saida WHERE saldo_f_sol = 0. "*-CS2025000249-04.07.2025-#181842-JT-inicio

  LOOP AT it_centro.
    DELETE t_saida WHERE vkbur EQ it_centro-bukrs.
  ENDLOOP.

  DATA lv_posnr TYPE posnr_va.

  "LOOP AT t_saida INTO wa_saida.

*    " 02.07.2025 - RAMON - 183689 -->
*    IF line_exists( it_0116[ vbeln = wa_saida-vbeln posnr = '000000' ] ).
*      lv_posnr = '000000'.
*      "lv_novo = abap_true.
*    ELSE.
*      lv_posnr = wa_saida-posnr.
*      "lv_novo = abap_false.
*    ENDIF.
*    " 02.07.2025 - RAMON - 183689 --<

*    READ TABLE it_0116 INTO DATA(wl_0116) WITH KEY vbeln = wa_saida-vbeln.
*
*    IF ( NOT sy-subrc IS INITIAL ) OR ( sy-subrc = 0 AND
*                                        wl_0116-status_workflow NE 'A'  AND
*                                        wl_0116-status_workflow IS NOT INITIAL ).
*      DELETE t_saida
*                  WHERE vbeln EQ wa_saida-vbeln AND
*                        posnr EQ wa_saida-posnr.
*    ENDIF.

  "ENDLOOP.

  "Teste DEV - inicio
  IF sy-sysid = 'DEV'.
    IF t_saida[] IS INITIAL.
      APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs>).

      <fs>-kunnr = '150'.

    ENDIF.
  ENDIF..
  "Teste DEV - fim


ENDFORM.                    " ORGANIZAR_DADOS

*&      Form  IMPRIMIR_DADOS
FORM imprimir_dados .
  PERFORM definir_eventos.
  PERFORM montar_layout.

  " 183689 - RAMON - 14.07.2025 -->
  PERFORM f_edit_mask_alv USING 'VLR_SOL'.
  PERFORM f_edit_mask_alv USING 'VLR_APR'.
  " 183689 - RAMON - 14.07.2025 --<

  wl_layout-colwidth_optimize = 'X'.
  wl_layout-info_fieldname = 'COLOR'.
  wl_layout-expand_fieldname = 'T_STYLE'.

*-IR037399  - jtassoni - 18.09.2020 - inicio
  PERFORM f_lock_table.
*-IR037399  - jtassoni - 18.09.2020 - fim

  IF l_user_lock IS NOT INITIAL.
    MESSAGE s000(fb) WITH 'Registro(s) bloqueado(s) pelo usuário:'
                          l_user_lock.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = v_report
      it_fieldcat        = estrutura[]
      is_layout          = wl_layout
*     i_save             = 'A'
      it_events          = events
    TABLES
      t_outtab           = t_saida.

ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.
  CASE wg_ucomm.
    WHEN ''.
      PERFORM f_carregar_eventos USING: slis_ev_user_command  'XUSER_COMMAND',
                                        slis_ev_pf_status_set 'XPF_STATUS_SET',
                                        slis_ev_top_of_page   'XTOP_OF_PAGE'.
    WHEN '&IC1'.
      PERFORM f_carregar_eventos USING: slis_ev_user_command  'XUSER_COMMAND_POP',
                                        slis_ev_pf_status_set 'XPF_STATUS_SET_POP'.
  ENDCASE.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  DATA c_x TYPE c.

  IF p_jsol EQ abap_true. "SOMENTE VISUALIZAÇÃO
    c_x = abap_false.
  ELSE.
    c_x = abap_true.
  ENDIF.


  CASE wg_ucomm.
    WHEN ''.
      PERFORM montar_estrutura USING:
        1  ''         ''        'T_SAIDA' 'CHECKBOX'        ''                    '01' c_x,
        2  'VBAK'     'KUNNR'   'T_SAIDA' 'KUNNR'           'Cliente'             ' ' ' ',
        3  'KNA1'     'NAME1'   'T_SAIDA' 'NAME1'           'Nome do Cliente'     '10' ' ',
        4  'VBAK'     'VKBUR'   'T_SAIDA' 'VKBUR'           ' '                   ' ' ' ',
        5  'ZSDT0040' 'SAFRA'   'T_SAIDA' 'SAFRA'           'Safra '              ' ' ' ',
        6  'ZSDT0040' 'CULTURA' 'T_SAIDA' 'CULTURA'         'Cultura '           ' ' ' ',
        7  'VBAP'     'VBELN'   'T_SAIDA' 'VBELN'           'Ordem de Venda'      ' ' ' ',
        8  'VBAP'     'POSNR'   'T_SAIDA' 'POSNR'           ' '                   ' ' ' ',
        9  'VBAP'     'MATNR'   'T_SAIDA' 'MATNR'           ' '                   ' ' ' ',
       10  'VBAP'     'ARKTX'   'T_SAIDA' 'ARKTX'           'Descrição Material'  '10' ' ',
       11  'MARA'     'WRKST'   'T_SAIDA' 'WRKST'           'Marca'               ' ' ' ',
"FF #173947 - inicio
       12  'VBKD'     'INCO1'   'T_SAIDA' 'INCOTERMS'       'Incoterms'           ' ' ' ',

"FF #173947 - fim
       13  'VBAP'     'WERKS'   'T_SAIDA' 'WERKS'           'Centro'              ' ' ' ',
       14  'VBAP'     'MEINS'   'T_SAIDA' 'MEINS'           'Unid.'               ' ' ' ',
       15  'VBAP'     'KWMENG'  'T_SAIDA' 'KWMENG'          'Qte. Ordem'          ' ' ' ',
       16  'KONV'     'KWERT'   'T_SAIDA' 'KWERT'           'Valor Total'         ' ' ' ',
       17  'VBAK'     'WAERK'   'T_SAIDA' 'WAERK'           'Moeda'               ' ' ' ',
       18  'VBFA'     'RFMNG'   'T_SAIDA' 'QTE_FATURADO'    'Qte. Faturado'       ' ' ' ',
       19  'VBAP'     'KWMENG'  'T_SAIDA' 'QTE_SOL'         'Qte. Solicitado'     ' ' ' ',
       20  'VBAP'     'KWMENG'  'T_SAIDA' 'QTE_DISPONIVEL'  'Qte. Disponível'     ' ' ' '.

      IF p_asol EQ abap_true.
        PERFORM montar_estrutura USING:
       21  'VBAP'     'KWMENG'      'T_SAIDA' 'QTE_LIBERAR'     'Quantidade'          ' ' c_x.
      ENDIF.

      PERFORM montar_estrutura USING:
       " 03.07.2025 - RAMON - 183689 -->
       22  'VBAP'     'NETWR_AP' 'T_SAIDA' 'VLR_APR'        'Vlr. Aprovado'       ' ' ' ',
       23  'VBAP'     'NETWR_AP' 'T_SAIDA' 'VLR_SOL'        'Vlr. Solicitado'     ' ' ' '.
      " 03.07.2025 - RAMON - 183689 --<

      IF p_asol EQ abap_true.
        PERFORM montar_estrutura USING:
       24  'VBAK'     'ERDAT'       'T_SAIDA' 'DT_ENTREGA'      'Data p/ Entrega'     ' ' c_x,

       25  ' '        ' '           'T_SAIDA' 'OBSERVACAO'      'Observação'          ' ' ' ',
       "22  ' '        ' '          'T_SAIDA' 'ROTEIRO'         'Roteiro'             ' ' ' ',
       26  'ZSDT0132' 'NR_ROT'      'T_SAIDA' 'NR_ROT'          'N. Roteiro'          '15' c_x,
       27  ' '        ' '           'T_SAIDA' 'ITINERARIO'      'Itinerário Rot.'     ' ' ' '. "FF #143875

      ENDIF.

      PERFORM montar_estrutura USING:
      28  'VBAP'     'ROUTE'   'T_SAIDA' 'ITINERARIO_OV'   'Itinerário OV'       ' ' ' ',
      "<<<------"169500 - NMS - INI------>>>
      29  ' '          'TP_OPER_D'   'T_SAIDA' 'TP_OPER_D'        'Tp. Oper.'          '20' c_x,
      "<<<------"169500 - NMS - FIM------>>>

      30  'VBAP'     'KWMENG'  'T_SAIDA' 'QTE_SALDO'       'Saldo Lt/Cg'         ' ' ' ',
      31  'VBAP'     'KWMENG'  'T_SAIDA' 'SALDO_F_OV'      'Sdo. a Fat. OV'      ' ' ' ',
      32  'VBAP'     'KWMENG'  'T_SAIDA' 'SALDO_F_SOL'     'Sdo. a Fat. SOL'     ' ' ' '.


    WHEN '&IC1'.

      FREE: it_fcat, wa_fcat.

      DEFINE popup.

        wa_fcat-col_pos   = &1.
        wa_fcat-ref_table = &2.
        wa_fcat-ref_field = &3.
        wa_fcat-tabname   = &4.
        wa_fcat-fieldname = &5.
        wa_fcat-reptext   = &6.
        wa_fcat-scrtext_s = &6.
        wa_fcat-scrtext_m = &6.
        wa_fcat-scrtext_l = &6.
        wa_fcat-outputlen = &7.
        wa_fcat-style     = &8.


    CASE wa_fcat-fieldname.
      WHEN 'NR_ROT'.
        wa_fcat-hotspot = 'X'.
      WHEN OTHERS.
    ENDCASE.

    APPEND wa_fcat TO it_fcat.
    CLEAR wa_fcat.

      END-OF-DEFINITION.

      popup:
            1 'ZSDT0082'  'NRO_SOL'     'IT_SAIDA2'  'NRO_SOL'    'Nº.Solic.'          '10' ' ',
            2 'ZSDT0082'  'DT_SOL'      'IT_SAIDA2'  'DT_SOL'     'Dt.Solic.'          '10' ' ',
            3 'ZSDT0082'  'VBELN'       'IT_SAIDA2'  'VBELN'      ' '                  ' '  ' ',
            4 'ZSDT0082'  'POSNR'       'IT_SAIDA2'  'POSNR'      ' '                  ' '  ' ',
            5 'ZSDT0082'  'QTE_SOL'     'IT_SAIDA2'  'QTE_SOL'    'Quantidade'         '20' ' ',
            6 'ZSDT0131'  'QTD_VINC'    'IT_SAIDA2'  'QTD_VINC'   'Qtd. Lote/Carga'    '15' ' ',
            6 'ZSDT0082'  'DT_ENTREGA'  'IT_SAIDA2'  'DT_ENTREGA' 'Data Requerida Ent' '10' ' ',
            7 'ZSDT0082'  'STATUS'      'IT_SAIDA2'  'STATUS'     'Status'             '15' ' ',
            8  ' '        ' '           'IT_SAIDA2'  'OBSERVACAO' 'Observação'         ' '  ' ',
            "9  ' '        ' '           'IT_SAIDA2'  'ROTEIRO'    'Roteiro'            ' '  ' ',
            10 'ZSDT0132' 'NR_ROT'      'IT_SAIDA2'  'NR_ROT'     'N. Roteiro'         ' '  ' ',
            11 'ZSDT0132' 'ROT_DESC'    'IT_SAIDA2'  'ROT_DESC'   'Desc. Roteiro'      '70' ' '.
**<<<------"169500 - NMS - INI------>>>
      popup:
           12 ' '         'TP_OPER_D'   'T_SAIDA2'    'TP_OPER_D'  'Tp. Oper.'          '20' ' '.
**<<<------"169500 - NMS - FIM------>>>
      LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
        CASE  <fcat>-fieldname.
          WHEN 'ROTEIRO' OR 'OBSERVACAO'.
            <fcat>-style = cl_gui_alv_grid=>mc_style_button.
        ENDCASE.
      ENDLOOP.

  ENDCASE.
ENDFORM.                    " MONTAR_LAYOUT

*&      Form  MONTAR_ESTRUTURA
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit).

  CLEAR wa_estrutura.

  wa_estrutura-edit          = p_edit.
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
  wa_estrutura-outputlen     = p_outputlen.

  IF p_outputlen IS NOT INITIAL.
    wa_estrutura-outputlen = p_outputlen.
  ENDIF.

  CASE p_field.
    WHEN 'QTE_SOL'.
      wa_estrutura-hotspot = 'X'.
  ENDCASE.

  IF p_field EQ 'CHECKBOX'.
    wa_estrutura-checkbox = 'X'.
  ENDIF.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA


*       FORM x_top_of_page                                            *
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE
*&      Form  F_CONSTRUIR_CABECALHO
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

*&      Form  INICIAR_VARIAVES
FORM iniciar_variaveis.
  v_report = sy-repid.
  PERFORM f_construir_cabecalho USING 'H' TEXT-002.

ENDFORM.                    " INICIAR_VARIAVES

*       FORM XPF_STATUS_SET                                            *
FORM xpf_status_set USING ucomm TYPE kkblo_t_extab.         "#EC CALLED
  DATA: tl_fcode TYPE TABLE OF sy-ucomm,
        wl_fcode TYPE sy-ucomm.

  DATA: gt_f4 TYPE lvc_t_f4.
  DATA: gs_f4 TYPE lvc_s_f4.

  DATA: gr_events       TYPE REF TO lcl_event_receiver,
        ls_sel_hide     TYPE slis_sel_hide_alv,
        it_fieldcatalog TYPE lvc_t_fcat,
        wa_fieldcatalog TYPE lvc_s_fcat,
        is_table        TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      es_sel_hide = ls_sel_hide
      e_grid      = ref1.

  CALL METHOD ref1->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = it_fieldcatalog.

  LOOP AT it_fieldcatalog INTO wa_fieldcatalog
    WHERE fieldname EQ 'OBSERVACAO'
       OR fieldname EQ 'TP_OPER_D'              "<<<------"169500 - NMS - FIM------>>>
       OR fieldname EQ 'ROTEIRO'.
**<<<------"169500 - NMS - INI------>>>
    IF wa_fieldcatalog-fieldname EQ 'TP_OPER_D'.
      wa_fieldcatalog-f4availabl = abap_on.

    ELSE.
**<<<------"169500 - NMS - FIM------>>>
      wa_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
**<<<------"169500 - NMS - INI------>>>
    ENDIF.
**<<<------"169500 - NMS - FIM------>>>
    MODIFY it_fieldcatalog FROM wa_fieldcatalog.
  ENDLOOP.

  CALL METHOD ref1->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fieldcatalog.


  is_table-row = 'X'.
  is_table-col = 'X'.

  CALL METHOD ref1->refresh_table_display
    EXPORTING
      is_stable      = is_table
      i_soft_refresh = 'X'.

  IF init IS INITIAL.
    CALL METHOD ref1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ref1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT gr_events.
    SET HANDLER: gr_events->handle_on_button_click FOR ref1,
                 gr_events->on_data_changed FOR ref1,
                 gr_events->on_f4 FOR ref1.

    gs_f4-fieldname = 'NR_ROT'.
    gs_f4-register = 'X'.
    gs_f4-getbefore = 'X'.
    gs_f4-chngeafter = 'X'.
    INSERT gs_f4 INTO TABLE gt_f4.
**<<<------"169500 - NMS - INI------>>>
    gs_f4-fieldname  = 'TP_OPER_D'.
    gs_f4-register   = abap_on.
    gs_f4-getbefore  = abap_on.
    gs_f4-chngeafter = abap_on.
    INSERT gs_f4 INTO TABLE gt_f4.
**<<<------"169500 - NMS - FIM------>>>
    CALL METHOD ref1->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4.

    init  = 'X'.
  ENDIF.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING tl_fcode.
ENDFORM. "XPF_STATUS_SET


*&      Form  XPF_STATUS_SET_POP
FORM xpf_status_set_pop USING ucomm TYPE kkblo_t_extab.     "#EC CALLED
  DATA: tl_fcode TYPE TABLE OF sy-ucomm,
        wl_fcode TYPE sy-ucomm.

  DATA: gr_events       TYPE REF TO lcl_event_receiver_pop,
        ls_sel_hide     TYPE slis_sel_hide_alv,
        it_fieldcatalog TYPE lvc_t_fcat,
        wa_fieldcatalog TYPE lvc_s_fcat,
        is_table        TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      es_sel_hide = ls_sel_hide
      e_grid      = ref2.

  CALL METHOD ref2->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = it_fieldcatalog.

  LOOP AT it_fieldcatalog INTO wa_fieldcatalog
  WHERE fieldname EQ 'OBSERVACAO'
     OR fieldname EQ 'ROTEIRO'.

    wa_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
    MODIFY it_fieldcatalog FROM wa_fieldcatalog.
  ENDLOOP.


  CALL METHOD ref2->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fieldcatalog.

  is_table-row = 'X'.
  is_table-col = 'X'.

  CALL METHOD ref2->refresh_table_display
    EXPORTING
      is_stable      = is_table
      i_soft_refresh = 'X'.

  CALL METHOD ref2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD ref2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CREATE OBJECT gr_events.
  SET HANDLER: gr_events->on_data_changed FOR ref2,
               gr_events->handle_on_button_click FOR ref2.

  SET PF-STATUS 'STATUS_POPUP' EXCLUDING tl_fcode.
ENDFORM. "XPF_STATUS_SET_POP


*&      Form  XUSER_COMMAND_POP
FORM xuser_command_pop USING ucomm    LIKE sy-ucomm
                         selfield TYPE kkblo_selfield..     "#EC CALLED
  DATA: wl_0082     TYPE zsdt0082,
        wl_index(3) TYPE n,
        p_number    TYPE zde_nro_sol,
        xqte_sol    TYPE zsdt0082-qte_sol.

  CASE ucomm.
    WHEN '&SAV'.
      LOOP AT it_saida2 INTO wa_saida2.
        IF wa_saida2-alt = 'X'.
          CLEAR: p_number.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'Z_NUM_SOL'
            IMPORTING
              number                  = p_number
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.

          IF sy-subrc NE 0.
            CLEAR: p_number.
            MESSAGE e836(sd) WITH 'O intervalo de numeração, não foi encontrado!'.

          ELSE.
            xqte_sol = wa_saida2-qte_sol * -1.

            MOVE: wa_saida2-nro_sol     TO wl_0082-nro_sol,
                  p_number              TO wl_0082-seq,
                  wa_saida2-vbeln       TO wl_0082-vbeln,
                  wa_saida2-posnr       TO wl_0082-posnr,
                  sy-datum              TO wl_0082-dt_sol,
                  xqte_sol              TO wl_0082-qte_sol,
                  0                     TO wl_0082-qte_lib,
                  sy-uname              TO wl_0082-usuario_sol,
                  wa_saida2-vkorg       TO wl_0082-vkorg,
                  wa_saida2-spart       TO wl_0082-spart,
                  wa_saida2-vkgrp       TO wl_0082-vkgrp,
                  wa_saida2-vkbur       TO wl_0082-vkbur,
                  wa_saida2-auart       TO wl_0082-auart.
            MOVE: wa_saida2-tp_oper     TO wl_0082-tp_oper.       "<<<------"169500 - NMS------>>>

            INSERT INTO zsdt0082 VALUES wl_0082.
          ENDIF.
        ENDIF.
      ENDLOOP.
      MESSAGE 'Solicitação inserida com sucesso.' TYPE 'I'.

*      Carrega os dados atualizados
      PERFORM selecionar_dados.

      " 23.07.2025 - RAMON - 183689 -->
      "PERFORM organizar_dados.
      PERFORM organizar_dados_2.
      " 23.07.2025 - RAMON - 183689 --<

      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "XUSER_COMMAND_POP

*       FORM XUSER_COMMAND                                            *
FORM xuser_command USING ucomm    LIKE sy-ucomm
                         selfield TYPE kkblo_selfield.

  DATA: tl_0082        TYPE TABLE OF zsdt0082,
        wl_0082        TYPE zsdt0082,
        p_number       TYPE zde_nro_sol,
        wl_header      TYPE thead,
        wl_index(3)    TYPE n,
        tl_tlines      LIKE tline OCCURS 0 WITH HEADER LINE,
        wg_index,
        xsdo           TYPE zsdt0082-qte_sol,
        xqtsol         TYPE zsdt0082-qte_sol,
        tqtsol(20)     TYPE c,
        xqlib          TYPE zsdt0082-qte_lib,
        tqlib(20)      TYPE c,
        nrosol         TYPE zsdt0082-nro_sol,
        lv_num_criadas TYPE c LENGTH 255,
        ls_sel_hide    TYPE slis_sel_hide_alv,
        is_table       TYPE lvc_s_stbl,
        erro(1),
        lv_erro        TYPE char01,
        msg_error(255),
        wa_zsdt0132    TYPE zsdt0132.

  DATA: tl_vbep   TYPE TABLE OF vbep WITH HEADER LINE,
        valida    TYPE char1,
        f_headinx LIKE bapisdh1x,
        tl_return TYPE TABLE OF bapiret2   WITH HEADER LINE.

  DATA: zcl_obj_manutencao_insumos TYPE REF TO zcl_manutencao_insumos.
  CREATE OBJECT zcl_obj_manutencao_insumos.
  DATA: vg_check_erro TYPE char01.

  DATA: BEGIN OF i_order_item_in OCCURS 0.
          INCLUDE STRUCTURE bapisditm.
  DATA: END   OF i_order_item_in.

  DATA: BEGIN OF i_order_item_inx OCCURS 0.
          INCLUDE STRUCTURE bapisditmx.
  DATA: END   OF i_order_item_inx.

  DATA: BEGIN OF i_sched OCCURS 10.
          INCLUDE STRUCTURE bapischdl.
  DATA: END OF i_sched.

  DATA: BEGIN OF i_schedx OCCURS 10.
          INCLUDE STRUCTURE bapischdlx.
  DATA: END OF i_schedx.

  DATA: r_werks TYPE RANGE OF werks_d.

  DATA: it_values  TYPE TABLE OF rgsb4.
**<<<------"169500 - NMS - INI------>>>
  DATA: tl_dias    TYPE STANDARD TABLE OF zsdt0012 WITH HEADER LINE.
**<<<------"169500 - NMS - FIM------>>>
  DATA: wa_values TYPE rgsb4,
        wa_werks  LIKE LINE OF r_werks.

  FREE: events, estrutura, it_saida2.

  REFRESH it_values.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = 'MV45AFZZ_WERKS'
      table         = 'VBAP'
      class         = '0000'
      fieldname     = 'WERKS'
    TABLES
      set_values    = it_values
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc IS INITIAL.

    LOOP AT it_values INTO wa_values.
      wa_werks = 'IEQ'.
      wa_werks-low    = wa_values-from.
      IF wa_values-to IS NOT INITIAL.
**<<<------"169500 - NMS - INI------>>>
*        wa_werks = 'IBT'.
        wa_werks(3) = 'IBT'.
**<<<------"169500 - NMS - FIM------>>>
        wa_werks-high = wa_values-to.
      ENDIF.

      APPEND wa_werks TO r_werks.
    ENDLOOP.
  ENDIF.

  wg_ucomm = ucomm.

  CASE ucomm.


      "FF #168675 - inicio
    WHEN '&MANUT_ROT'. "Botão Manutenção do Roteiro // Status gui

      DATA: lv_cont TYPE i.

      LOOP AT t_saida INTO DATA(wa) WHERE checkbox IS NOT INITIAL.
        DATA(lv_tabix) = sy-tabix.
        lv_cont = lv_cont + 1.
      ENDLOOP.

      IF lv_cont > 1.
        MESSAGE e836(sd) WITH 'Selecione apenas uma linha para Manutenção do Roteiro.' DISPLAY LIKE 'I'.
        STOP.
      ENDIF.

      READ TABLE t_saida INTO DATA(wa_saida) WITH KEY checkbox = abap_true.
      IF sy-subrc <> 0.
        MESSAGE e836(sd) WITH 'Selecione uma linha para Manutenção do Roteiro.' DISPLAY LIKE 'I'.
      ELSE.

        UNPACK wa_saida-kunnr TO wa_saida-kunnr.

        SUBMIT zsdr0061
          WITH p_kunnr = wa_saida-kunnr
          AND RETURN.


      ENDIF.
      "FF #168675 - fim


      "FF #143875 - inicio
    WHEN '&CRIAR_ITI'. "Botão criar itinerario

      DATA: lv_contador TYPE i.


      LOOP AT t_saida INTO wa WHERE checkbox IS NOT INITIAL.
        lv_tabix = sy-tabix.
        lv_contador = lv_contador + 1.
      ENDLOOP.

      IF lv_contador > 1.
        MESSAGE e836(sd) WITH 'Selecione apenas uma linha para criar o itinerário.' DISPLAY LIKE 'I'.
        STOP.
      ENDIF.


      READ TABLE t_saida INTO wa_saida WITH KEY checkbox = abap_true.
      IF sy-subrc <> 0.
        MESSAGE e836(sd) WITH 'Selecione uma linha para criar o itinerário.' DISPLAY LIKE 'I'.
      ELSE.

        SELECT SINGLE *
          FROM zsdt0132
          INTO @DATA(ls_132)
          WHERE nr_rot EQ @wa_saida-nr_rot.
        IF sy-subrc <> 0.
          CLEAR ls_132.
        ENDIF.

      ENDIF.

      IF wa_saida-itinerario IS NOT INITIAL .
        MESSAGE e836(sd) WITH 'Já existe itinerario para a linha selecionada.' DISPLAY LIKE 'I' .
      ENDIF.

      IF wa_saida-nr_rot IS INITIAL OR ls_132-lzone IS INITIAL.
        MESSAGE e836(sd) WITH 'Cadastrar Roteiro/Zona Transp. p/criar itinerario.' DISPLAY LIKE 'I'.
      ENDIF.

      READ TABLE t_vbpa WITH KEY vbeln = wa_saida-vbeln INTO DATA(wa_vbpa).
      IF sy-subrc <> 0.
        CLEAR wa_vbpa.
      ENDIF.

*    SET PARAMETER ID 'ZONA_ORI' FIELD wa_lfa1_pc-lzone.
      SET PARAMETER ID 'ZONA_DES' FIELD ls_132-lzone.
      SET PARAMETER ID 'COD_CLI'  FIELD ls_132-kunnr.
      SET PARAMETER ID 'COD_PC'   FIELD wa_vbpa-lifnr.


      SUBMIT zlesr0162 AND RETURN WITH p_kunnr = ls_132-kunnr
                                  WITH p_lifnr = wa_vbpa-lifnr
                                  WITH p_lzone = ls_132-lzone.

      SELECT SINGLE route
      FROM trolz
      WHERE azone = @wa_vbpa-lzone
        AND lzone = @ls_132-lzone
        AND route <> @abap_off   "*-US190671-15.09.2025-#190671-JT
        INTO @DATA(lv_route).

      IF sy-subrc = 0.
        wa_saida-itinerario = lv_route.
      ELSE.
        CLEAR wa_saida-itinerario.
      ENDIF.


      MODIFY t_saida FROM wa_saida INDEX lv_tabix.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          es_sel_hide = ls_sel_hide
          e_grid      = ref1.

      CALL METHOD ref1->refresh_table_display
        EXPORTING
          is_stable = is_table.

      CALL METHOD: cl_gui_cfw=>dispatch,
                   cl_gui_cfw=>flush.

      "FF #143875 - fim

    WHEN '&SOL_EMBAR'.

      DATA: lv_dt_entrega  TYPE zsdt0082-dt_entrega,        "FF #145609
            lv_qte_sol     TYPE vbap-kwmeng,                "FF #145609
            lv_qte_sdo_lib TYPE vbap-kwmeng,                "FF #145609
            lv_tpope       TYPE zsded_tpopr.

      CLEAR: wa_saida, wa_aux.

      IF p_extcal IS NOT INITIAL.

        lv_dt_entrega  = p_dt_en.
        lv_qte_sol     = p_qte_1.
        lv_qte_sdo_lib = p_qte_2.
        lv_tpope       = p_tpope.

*        IMPORT _SAIDA-DT_ENTREGA  TO LV_DT_ENTREGA  FROM MEMORY ID 'memory_dt_entrega'. "Export feito no programa ZSDR0038  "FF #145609
*        IMPORT _SAIDA-QTE_SOL     TO LV_QTE_SOL     FROM MEMORY ID 'memory_qte_sol'. "Export feito no programa ZSDR0038  "FF #145609
*        IMPORT _SAIDA-QTE_SDO_LIB TO LV_QTE_SDO_LIB FROM MEMORY ID 'memory_qte_sdo_lib'. "Export feito no programa ZSDR0038  "FF #145609
      ENDIF.

      READ TABLE t_saida TRANSPORTING NO FIELDS WITH KEY checkbox = abap_true.
      CHECK sy-subrc IS INITIAL.

      LOOP AT t_saida INTO wa_saida WHERE checkbox IS NOT INITIAL.

        IF p_extcal IS NOT INITIAL.
          wa_saida-dt_entrega  = lv_dt_entrega.
          wa_saida-qte_sol     = lv_qte_sol.
          wa_saida-qte_liberar = lv_qte_sdo_lib.
          wa_saida-tp_oper     = lv_tpope.
        ENDIF.

        IF wa_saida-dt_entrega IS INITIAL.
          MESSAGE e024(sd) WITH 'Informar data de entrega'.

        ELSE.
* Verifica tabela exceção.
          IF wa_saida-spart EQ '03' OR wa_saida-spart EQ '04'. "Defensivos/Sementes

**<<<------"169500 - NMS - INI------>>>
* Valida cadastro no Sisdev.
*            perform zf_valida_sisdev using wa_saida-kunnr.
            CLEAR: vg_check_erro.
            zcl_obj_manutencao_insumos->check_cadastro_forn_sisdev(
              EXPORTING
                i_werks = wa_saida-werks " Centro
                i_kunnr = wa_saida-kunnr " Nº cliente
              IMPORTING
                e_erro  = vg_check_erro                 " Campo de texto do comprimento 1
            ).

            IF  vg_check_erro IS NOT INITIAL.
              MESSAGE 'Cliente não possuem cadastro no SISDEV' TYPE 'E'.
              EXIT.
            ENDIF.
* Verifica a data de entrega
          ENDIF.

          IF p_extcal IS INITIAL. "// P_EXTCAL se estiver preechido o processo é via BACK da transação ZSDT0081 "// WBARBOSA 02/09/2025
            IF wa_saida-spart EQ '04'.
              SELECT SINGLE * FROM zsdt0348 INTO @DATA(ws_zsdt0348)
                WHERE tcode EQ @sy-tcode
                  AND uname EQ @sy-uname.

              IF NOT sy-subrc IS INITIAL.
                DATA: vl_add_days TYPE fiappl_arrear,
                      vl_dateref  TYPE sy-datum.
                SELECT * FROM zsdt0012 INTO TABLE tl_dias ORDER BY date_create DESCENDING time_create DESCENDING.
                READ TABLE tl_dias INTO DATA(el_dias) INDEX 1.
                vl_add_days = el_dias-dias.
* Calcula a data nforme a quantidade de dias informada.
                CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
                  EXPORTING
                    i_date      = sy-datum
                    i_days      = vl_add_days
                    signum      = '+'
                  IMPORTING
                    e_calc_date = vl_dateref.

                IF wa_saida-dt_entrega < vl_dateref.
*              vl_dateref = vl_dateref - 1.
                  MESSAGE e024(sd) WITH 'Informar a data de entrega após o dia' vl_dateref.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF. "// WBARBOSA 02/09/2025
        ENDIF.
**<<<------"169500 - NMS - FIM------>>>
        "FF #145609 - inicio

        CLEAR wa_vbap.
        READ TABLE t_vbap WITH KEY vbeln = wa_saida-vbeln
                                   posnr = wa_saida-posnr INTO wa_vbap.

        "FF #145609 - fim


        "FF #143875 - inicio
        IF wa_saida-itinerario IS INITIAL AND wa_saida-itinerario_ov <> 'FOB'. "FF #173947
          msg_error = |Não existe um itinerário para o roteiro selecionado, gere um Itinerário para prosseguir.|.
          EXIT.
        ENDIF.
        "FF #143875 - fim


        IF wa_saida-dt_entrega IS INITIAL.
          msg_error = |É obrigatorio o preenchimento' ' do campo Data Requerida.|.
          EXIT.
        ENDIF.

*-US191683-25.09.2025-#191683-JT-inicio
        IF wa_saida-spart = '03'.
          PERFORM f_valida_propr_indea USING wa_saida-vkorg  "*-US192384-03.10.2025-#192384-JT
                                             wa_saida-kunnr
                                             wa_saida-werks
                                             wa_saida-nr_rot
                                    CHANGING lv_erro.
          IF lv_erro = abap_true.
            RETURN.
          ENDIF.
        ENDIF.
*-US191683-25.09.2025-#191683-JT-fim



*        READ TABLE T_AUX INTO WA_AUX WITH KEY VBELN = WA_SAIDA-VBELN
*                                              POSNR = WA_SAIDA-POSNR
*                                              VKORG = WA_SAIDA-VKORG
*                                              SPART = WA_SAIDA-SPART
*                                              VKGRP = WA_SAIDA-VKGRP
*                                              VKBUR = WA_SAIDA-VKBUR
*                                              TIPO  = 'ROTEIRO'.
*
*        IF WA_AUX-TDLINE IS INITIAL.
*          MSG_ERROR = |Preencher informações de Roteiro para OV' { WA_SAIDA-VBELN }|.
*          EXIT.
*        ENDIF.
        IF wa_saida-itinerario_ov <> 'FOB'. "FF #173947.

          IF wa_saida-nr_rot IS INITIAL AND
            msg_error = |É obrigatorio o preenchimento do campo N. Roteiro.|.
            EXIT.
          ENDIF.

          SELECT SINGLE *
            FROM zsdt0132
            INTO wa_zsdt0132
            WHERE nr_rot EQ wa_saida-nr_rot.

          IF wa_zsdt0132-kunnr NE wa_saida-kunnr.
            msg_error = |Roteiro não pertence ao Cliente|.
            EXIT.
          ENDIF.

        ENDIF.

        IF wa_saida-qte_liberar GT wa_saida-qte_disponivel.
          msg_error = |Quantidade solicitada maior que a quantidade disponível para OV' { wa_saida-vbeln }|.
          CLEAR: wa_saida-qte_sol.
          EXIT.
        ENDIF.

        IF wa_saida-qte_liberar LE 0.
          msg_error = 'Quantidade solicitada não é Superior a Zero!'.
          EXIT.
        ENDIF.

        " 183689 - RAMON - 14.07.2025 -->
        IF wa_saida-tp_oper = '00'.
          msg_error =  'Inserir Tp. Operação'.
          EXIT.
        ENDIF.
        " 183689 - RAMON - 14.07.2025 --<


        " 02.07.2025 - RAMON -- 183689 -->

        "IF wa_saida-proc_novo IS NOT INITIAL.

        CALL FUNCTION 'Z_OV_CHECK_SALDO_LIBERACAO'
          EXPORTING
            iv_vbeln     = wa_saida-vbeln
            iv_posnr     = wa_saida-posnr
            iv_qtde      = wa_saida-qte_liberar
            iv_proc_novo = wa_saida-proc_novo
          IMPORTING
            ev_erro      = erro
            ev_msgx      = msg_error.

        IF erro = abap_true.
          EXIT.
        ENDIF.

        "ENDIF.

        " 02.07.2025 - RAMON -- 183689 --<

        CLEAR: p_number.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'Z_NUM_SOL'
          IMPORTING
            number                  = p_number
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        IF sy-subrc NE 0.
          MESSAGE e836(sd) WITH 'O intervalo de numeração, não foi encontrado!'.

        ELSE.

          "FF #143875 - inicio
          CLEAR wa_vbpa.
          READ TABLE t_vbpa WITH KEY vbeln = wa_saida-vbeln INTO wa_vbpa.
          IF sy-subrc <> 0.
            CLEAR wa_vbpa.
          ENDIF.
          "FF #143875 - fim

          EXPORT p_number FROM p_number TO MEMORY ID 'memory_solic_emb'. "Import será feito na transação ZSDT0038     "FF #145609 - fim

          CLEAR: wl_index.

          ADD 1 TO wl_index.

          MOVE: p_number            TO wl_0082-nro_sol,
                wl_index            TO wl_0082-seq,
                wa_saida-vbeln      TO wl_0082-vbeln,
                wa_saida-posnr      TO wl_0082-posnr,
                ''                  TO wl_0082-seq_lib,
                wa_saida-vkorg      TO wl_0082-vkorg,
                wa_saida-spart      TO wl_0082-spart,
                wa_saida-vkgrp      TO wl_0082-vkgrp,
                wa_saida-vkbur      TO wl_0082-vkbur,
                wa_saida-auart      TO wl_0082-auart,
                wa_saida-werks      TO wl_0082-werks,
                sy-datum            TO wl_0082-dt_sol,
                wa_saida-qte_liberar TO wl_0082-qte_sol,
                sy-uname            TO wl_0082-usuario_sol,
                ''                  TO wl_0082-dt_liber,
                ''                  TO wl_0082-qte_lib,
                ''                  TO wl_0082-usuario_lib,
                1                   TO wl_0082-status,
                wa_saida-dt_entrega TO wl_0082-dt_entrega,
                wa_saida-nr_rot     TO wl_0082-nr_rot.
**<<<------"169500 - NMS - INI------>>>
          MOVE: wa_saida-tp_oper     TO wl_0082-tp_oper.
**<<<------"169500 - NMS - FIM------>>>

          CONCATENATE wl_0082-nro_sol wl_0082-seq INTO wl_header-tdname.

          wl_header-tdobject = 'ZTEXTO'.
          wl_header-tdid     = 'OBSE'.
          wl_header-tdspras  = sy-langu.
          PERFORM save_text USING 'OBS'
                                  wl_header.

          wl_header-tdobject = 'ZTEXTO'.
          wl_header-tdid     = 'ROTE'.
          wl_header-tdspras  = sy-langu.
          PERFORM save_text USING 'ROTEIRO'
                                  wl_header.

          "FF #143875 - inicio
          wl_0082-desmembrar = abap_false.
          IF wa_saida-itinerario IS NOT INITIAL AND wa_saida-itinerario_ov <> 'FOB'. "FF #173947..

            IF wa_saida-itinerario <> wa_vbap-route AND
               wa_saida-spart NE '03'. "Nao se aplica para Defensivos "SD - Ajuste Trava Itinerario ZSDT0079 US 191705 - WPP

              wl_0082-desmembrar = abap_true.
              wl_0082-route_iti = wa_saida-itinerario.
              wl_0082-route_ov = wa_vbap-route.

            ENDIF.
          ENDIF.
          "FF #143875 - fim

          APPEND wl_0082 TO tl_0082.

          IF wl_0082-desmembrar = abap_false.               "FF #143875

*          IF ( wa_saida-werks EQ '0175' AND wa_saida-mtart EQ 'ZFER' )
**<<<------"169500 - NMS - INI------>>>
*            IF ( wa_saida-werks IN r_werks AND wa_saida-mtart EQ 'ZFER' )
*               OR ( wa_saida-spart EQ '04' ).
            IF ( wa_saida-werks IN r_werks  AND "Verifica se o Centro faz parte
                 wa_saida-mtart EQ 'ZFER' ).    "Tipo material Fertilizante
**<<<------"169500 - NMS - FIM------>>>
              MOVE: wl_index          TO wl_0082-seq_lib.

              ADD 1 TO wl_index.

              MOVE:
                   wl_index          TO wl_0082-seq,
                   ''                TO wl_0082-dt_sol,
                   ''                TO wl_0082-qte_sol,
                   ''                TO wl_0082-usuario_sol,
                   sy-datum          TO wl_0082-dt_liber,
              wa_saida-qte_liberar   TO wl_0082-qte_lib,
                   sy-uname          TO wl_0082-usuario_lib,
                   wa_saida-nr_rot   TO wl_0082-nr_rot,
                   2                 TO wl_0082-status.

              APPEND wl_0082 TO tl_0082.

            ENDIF.
          ENDIF.

          IF lv_num_criadas IS INITIAL.
            lv_num_criadas = p_number.
          ELSE.
            CONCATENATE lv_num_criadas p_number INTO lv_num_criadas SEPARATED BY ','.
          ENDIF.

          CLEAR: wl_0082.
        ENDIF.
        CLEAR: wa_saida.

      ENDLOOP.

      MODIFY zsdt0082 FROM TABLE tl_0082.
      COMMIT WORK.

      CASE sy-dbcnt.
        WHEN 0.
          MESSAGE msg_error TYPE 'E'.
        WHEN 1.

          IF p_extcal <> 'X'. "Se for chamada externa, as mensgens estão sendo tratadas no programa chamador ZSDR0038

            MESSAGE s836(sd) WITH 'A Solicitação de embarque foi criada com número ' p_number.

          ENDIF.

        WHEN OTHERS.
          MESSAGE s836(sd) WITH 'As seguintes solicitações de embarque foram criadas: ' lv_num_criadas '.'.
      ENDCASE.

      LOOP AT tl_0082 INTO wl_0082.

        SELECT COUNT(*)
          FROM vbap AS a
          INNER JOIN vbep AS c
          ON c~vbeln EQ a~vbeln "USER STORY 131069 - MMSILVA - 15.01.2025
          AND c~posnr EQ a~posnr"USER STORY 131069 - MMSILVA - 15.01.2025
          INNER JOIN mara AS b ON a~matnr EQ b~matnr
          WHERE a~vbeln EQ wl_0082-vbeln
            AND a~posnr EQ wl_0082-posnr
            AND b~mtart EQ 'ZFER'
            AND c~lifsp <> '12'. "USER STORY 131069 - MMSILVA - 15.01.2025
        CHECK sy-subrc IS NOT INITIAL.

        SELECT * FROM vbep
            INTO TABLE tl_vbep
          WHERE vbeln EQ wl_0082-vbeln
            AND posnr EQ wl_0082-posnr
            AND lifsp NE '12'. "USER STORY 131069 - MMSILVA - 15.01.2025

        REFRESH: tl_return,i_order_item_in,i_order_item_inx,i_sched,i_schedx.
        CLEAR: f_headinx, valida.
        f_headinx-updateflag = 'U'.

        LOOP AT tl_vbep.
          i_sched-itm_number = wl_0082-posnr.
          i_sched-sched_line = tl_vbep-etenr.
          IF tl_vbep-lifsp = '10'.
            i_sched-req_dlv_bl  = ''.
            valida = 'X'.
          ENDIF.
          i_schedx-req_dlv_bl  = 'X'.
          i_schedx-itm_number = wl_0082-posnr.
          i_schedx-sched_line = tl_vbep-etenr.
          i_schedx-updateflag  = 'U'.
          APPEND: i_sched, i_schedx.
        ENDLOOP.

        IF valida EQ 'X'.
          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
            EXPORTING
              salesdocument    = wl_0082-vbeln
              order_header_inx = f_headinx
            TABLES
              return           = tl_return
              order_item_in    = i_order_item_in
              order_item_inx   = i_order_item_inx
              schedule_lines   = i_sched
              schedule_linesx  = i_schedx.

          READ TABLE tl_return WITH KEY type = 'E'.
          IF sy-subrc NE 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDIF.
      ENDLOOP.

*-CS2020001303 - 16.10.2021 - JT - inicio
      LOOP AT tl_0082 INTO wl_0082 WHERE qte_sol <> 0.
        PERFORM f_envia_carguero USING wl_0082-vbeln
                                       wl_0082-posnr.
      ENDLOOP.
*-CS2020001303 - 16.10.2021 - JT - fim

      PERFORM: selecionar_dados,
               organizar_dados_2. " 29.08.2025

      CHECK p_extcal IS INITIAL. "Só executar o ALV se não for a chamada externa

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          es_sel_hide = ls_sel_hide
          e_grid      = ref1.

      CALL METHOD ref1->refresh_table_display
        EXPORTING
          is_stable = is_table.

      CALL METHOD: cl_gui_cfw=>dispatch,
                   cl_gui_cfw=>flush.

    WHEN '&IC1'.

      CLEAR:wg_index, wa_saida_aux.
      FREE: it_saida2, wa_saida.

      READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.
      IF sy-subrc IS INITIAL.

        PERFORM seleciona_dados_popup USING wa_saida-vbeln wa_saida-posnr nrosol wa_saida-spart.

        PERFORM montar_layout.

        CALL SCREEN 0100 ENDING AT 118 10
                         STARTING AT 3 3.

        CHECK NOT var_refresh IS INITIAL.
        CLEAR var_refresh.

        CALL METHOD wa_alv->refresh_table_display.

        PERFORM: selecionar_dados,
                 organizar_dados_2. " 29.08.2026

        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            es_sel_hide = ls_sel_hide
            e_grid      = ref1.

        CALL METHOD ref1->refresh_table_display
          EXPORTING
            is_stable = is_table.

        CALL METHOD: cl_gui_cfw=>dispatch,
                     cl_gui_cfw=>flush.

      ENDIF.
  ENDCASE.
ENDFORM. "XUSER_COMMAND

*&---------------------------------------------------------------------*
*&  envia carguero
*&---------------------------------------------------------------------*
FORM f_envia_carguero USING p_vbeln
                            p_posnr.

  DATA: t_vbap_sel   TYPE TABLE OF vbap,
        l_nome_tvarv TYPE tvarvc-name,
        l_tot_brgew  TYPE vbap-brgew,
        l_lifnr      TYPE vbpa-lifnr,
        l_erro       TYPE char1,
        l_erro_mesg  TYPE bapi_msg.

  RANGES: r_vbpa        FOR vbpa-lifnr,
          r_matkl       FOR vbap-matkl.

  FREE: r_vbpa.

*--------------------
*-item OV
*--------------------
  SELECT a~*
    FROM vbap AS a
    INNER JOIN vbep AS b
     ON b~vbeln EQ a~vbeln "USER STORY 131069 - MMSILVA - 15.01.2025
     AND b~posnr EQ a~posnr"USER STORY 131069 - MMSILVA - 15.01.2025
    INTO TABLE @t_vbap_sel
   WHERE a~vbeln = @p_vbeln
     AND a~posnr = @p_posnr
     AND b~lifsp <> '12'. "USER STORY 131069 - MMSILVA - 15.01.2025

  CHECK sy-subrc = 0.

  READ TABLE t_vbap_sel INTO DATA(w_vbap) INDEX 1.

*--------------------
*-parceiro
*--------------------
  SELECT *
    FROM vbpa
    INTO @DATA(w_vbpa)
      UP TO 1 ROWS
   WHERE vbeln = @p_vbeln
     AND parvw = 'PC'.
  ENDSELECT.

  CHECK sy-subrc = 0.

  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(t_grp_mat)
   WHERE name = 'MAGGI_GR_FERTILIZANTES'.

  LOOP AT t_grp_mat INTO DATA(w_grp_mat).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_grp_mat-low ) TO r_matkl.
  ENDLOOP.


*--------------------
*-TVARV
*--------------------
  l_nome_tvarv = 'PARCEIRO_PC_CENTRO_' && w_vbap-werks.

  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(t_parceiro)
   WHERE name = @l_nome_tvarv.

  IF sy-subrc = 0.
    LOOP AT t_parceiro INTO DATA(w_parceiro).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_parceiro-low
        IMPORTING
          output = l_lifnr.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = l_lifnr ) TO r_vbpa.
    ENDLOOP.
  ENDIF.

  CHECK w_vbpa-lifnr IN r_vbpa[] AND r_vbpa[] IS NOT INITIAL.
  CHECK w_vbap-matkl IN r_matkl[] AND r_matkl[] IS NOT INITIAL.

*--------------------
*-tipo de material
*--------------------
  SELECT mtart
    FROM mara
    INTO @DATA(l_mtart)
      UP TO 1 ROWS
   WHERE matnr = @w_vbap-matnr.
  ENDSELECT.

  CHECK l_mtart = 'ZFER'.

*--------------------
*-zsdt0082
*--------------------
  SELECT vbeln, posnr, qte_lib, nr_rot
    FROM zsdt0082
    INTO TABLE @DATA(t_0082_tab)
   WHERE   vbeln  = @p_vbeln
     AND   posnr  = @p_posnr
     AND ( status = '2'
      OR   status = '5' ).

*--------------------
* peso total
*--------------------
  FREE l_tot_brgew.

  LOOP AT t_0082_tab INTO DATA(w_0082_tab).
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = w_vbap-matnr
        i_in_me              = w_vbap-gewei
        i_out_me             = 'KG'
        i_menge              = w_0082_tab-qte_lib
      IMPORTING
        e_menge              = w_0082_tab-qte_lib
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    l_tot_brgew = l_tot_brgew + w_0082_tab-qte_lib.
  ENDLOOP.

*--------------------
* ajusta vbap
*--------------------
  w_vbap-brgew         = l_tot_brgew.
  MODIFY t_vbap_sel FROM w_vbap INDEX 1.

*--------------------
* envia carguero
*--------------------
  FREE: l_erro, l_erro_mesg.

  TRY .
      zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_envia_fertilizantes_ov(
        EXPORTING
          i_vbeln     = p_vbeln
          t_vbap      = t_vbap_sel
        IMPORTING
          e_erro      = l_erro
          e_erro_mesg = l_erro_mesg ).

    CATCH zcx_integracao.
    CATCH zcx_error.
  ENDTRY.

  COMMIT WORK AND WAIT.

  IF l_erro = abap_true.
    MESSAGE i024(sd) WITH 'Envio Carguero: ' l_erro_mesg(50)
                                             l_erro_mesg+50(50)
                                             l_erro_mesg+100(100).
  ENDIF.

ENDFORM.

************************************************************************
*&      Form  SAVE_TEXT
************************************************************************
FORM save_text  USING    VALUE(p_2350)
                         wl_header TYPE thead.

  DATA: tl_tlines LIKE tline OCCURS 0 WITH HEADER LINE.

  REFRESH: tl_tlines.

  LOOP AT t_aux INTO wa_aux
    WHERE vbeln EQ wa_saida-vbeln
       AND posnr EQ wa_saida-posnr
       AND vkorg EQ wa_saida-vkorg
       AND spart EQ wa_saida-spart
       AND vkgrp EQ wa_saida-vkgrp
       AND vkbur EQ wa_saida-vkbur
       AND tipo  EQ p_2350.

    MOVE: '*'           TO tl_tlines-tdformat,
          wa_aux-tdline TO tl_tlines-tdline.

    APPEND tl_tlines.
    CLEAR tl_tlines.

  ENDLOOP.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = wl_header
      savemode_direct = 'X'
    TABLES
      lines           = tl_tlines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.

ENDFORM.                    " SAVE_TEXT
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

  DATA obj_events TYPE REF TO zcl_events.

  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name = 'C_POPUP'.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent = wa_cont.

    CREATE OBJECT obj_events
      EXPORTING
        io_alv_grid = wa_alv.

    SET HANDLER:
                 obj_events->on_handle                FOR wa_alv,
                 obj_events->on_toolbar               FOR wa_alv,
                 obj_events->on_but_clk               FOR wa_alv,
                 obj_events->handle_hotspot_click     FOR wa_alv.

    CLEAR: wa_layout, wa_variante.

    wa_layout-zebra      = abap_true.
    wa_layout-no_rowins  = abap_true.
    wa_layout-stylefname = 'cellstyles'.
    wa_layout-info_fname = 'LINE_COLOR'.
    wa_layout-sel_mode   = 'C'.
    wa_stable-row        = abap_true.

    wa_variante-report  = sy-repid.

    CALL METHOD wa_alv->set_table_for_first_display
      EXPORTING
        is_layout       = wa_layout
        is_variant      = wa_variante
        i_save          = 'X'
      CHANGING
        it_outtab       = it_saida2
        it_fieldcatalog = it_fcat.
  ELSE.
    CALL METHOD wa_alv->refresh_table_display.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_POPUP
*&---------------------------------------------------------------------*
FORM seleciona_dados_popup  USING p_vbeln
                                  p_posnr
                                  p_nrosol
                                  p_spart.

  DATA: it_zsdt0132 TYPE STANDARD TABLE OF zsdt0132,
        wa_zsdt0132 TYPE zsdt0132.

  SELECT *
   FROM zsdt0082
    INTO CORRESPONDING FIELDS OF TABLE it_0082
      WHERE vbeln EQ p_vbeln
       AND  posnr EQ p_posnr
       AND  seq   EQ 1.

*-CS2021000218-13.09.2022-#90108-JT-inicio
*-------------------------------
*- verifica se solicitacao esta cancelada
*-------------------------------
  SELECT *
    FROM zsdt0082
    INTO TABLE @DATA(tc_0082)
   WHERE vbeln  = @p_vbeln
     AND posnr  = @p_posnr
     AND status = '3'.

  LOOP AT it_0082 INTO wa_0082.
    DATA(l_tabix) = sy-tabix.
    READ TABLE tc_0082 INTO DATA(wc_0082) WITH KEY nro_sol = wa_0082-nro_sol
                                                   vbeln   = wa_0082-vbeln
                                                   posnr   = wa_0082-posnr.
    IF sy-subrc = 0.
      DELETE it_0082 INDEX l_tabix.
    ENDIF.
  ENDLOOP.
*-CS2021000218-13.09.2022-#90108-JT-fim

  SORT: it_0082 BY nro_sol.

  SELECT *
    FROM zsdt0132
    INTO TABLE it_zsdt0132
    FOR ALL ENTRIES IN it_0082
    WHERE nr_rot EQ it_0082-nr_rot.

  TRY .

      LOOP AT it_0082 INTO wa_0082.

        CLEAR: wa_saida2.

        MOVE:
          wa_0082-nro_sol    TO wa_saida2-nro_sol,
          wa_0082-dt_sol     TO wa_saida2-dt_sol,
          wa_0082-vbeln      TO wa_saida2-vbeln,
          wa_0082-posnr      TO wa_saida2-posnr,
          wa_0082-qte_sol    TO wa_saida2-qte_sol,
          wa_0082-dt_entrega TO wa_saida2-dt_entrega,
          wa_0082-nr_rot     TO wa_saida2-nr_rot.

        IF wa_0082-nro_sol IS NOT INITIAL.
          PERFORM busca_volume USING wa_0082-nro_sol
                                     wa_0082-seq
                                     p_spart
                               CHANGING wa_0082-qtd_vinc.
        ENDIF.

        MOVE:
          wa_0082-qtd_vinc     TO wa_saida2-qtd_vinc.

        IF wa_0082-nr_rot IS NOT INITIAL.
          READ TABLE it_zsdt0132 INTO wa_zsdt0132 WITH KEY nr_rot = wa_0082-nr_rot.
          IF sy-subrc IS INITIAL.
            MOVE wa_zsdt0132-rot_desc TO wa_saida2-rot_desc.
          ENDIF.
          CLEAR: wa_zsdt0132.
        ELSE.
          MOVE '9999999999' TO wa_saida2-nr_rot.
        ENDIF.

        CONCATENATE wa_0082-nro_sol wa_0082-seq INTO wl_name.
        "PERFORM READ_TEXT USING 'ROTE' WL_NAME.
        PERFORM read_text USING 'OBSE' wl_name.

        CASE wa_0082-status.
          WHEN 3.
            MOVE TEXT-003 TO wa_saida2-status.
          WHEN 2.
            MOVE TEXT-009 TO wa_saida2-status.
          WHEN 1.
            MOVE TEXT-004 TO wa_saida2-status.
        ENDCASE.

        IF p_nrosol <> wa_0082-nro_sol.
          APPEND wa_saida2 TO it_saida2.
        ENDIF.

        p_nrosol = wa_0082-nro_sol.

        FREE tg_texto.
**<<<------"169500 - NMS - INI------>>>
* Descrição do Tipo de Operação.
        READ TABLE tg_tpopr INTO DATA(el_tpopr) WITH KEY domvalue_l = wa_saida2-tp_oper.
        IF sy-subrc IS INITIAL.
          wa_saida2-tp_oper_d = el_tpopr-ddtext.

        ELSE.
          CLEAR wa_saida2-tp_oper_d.

        ENDIF.
**<<<------"169500 - NMS - FIM------>>>
      ENDLOOP.
    CATCH cx_root.
      MESSAGE e000(o0) WITH 'Erro ao abrir janela.'.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM read_text  USING p_id p_name.

  MOVE p_id    TO wl_id.
  MOVE p_name TO wl_name.

  FREE tg_texto.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = wl_id
      language                = sy-langu
      name                    = wl_name
      object                  = 'ZTEXTO'
    TABLES
      lines                   = tg_texto
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc IS INITIAL.

    IF wl_id EQ 'ROTE'.
      MOVE icon_display_more TO wa_saida-roteiro.
      MOVE icon_display_more TO wa_saida2-roteiro.
    ELSE.
      MOVE icon_display_more TO wa_saida-observacao.
      MOVE icon_display_more TO wa_saida2-observacao.
    ENDIF.

  ELSE.

    IF wl_id EQ 'ROTE'.
      MOVE icon_enter_more TO wa_saida-roteiro.
      MOVE icon_enter_more TO wa_saida2-roteiro.
    ELSE.
      MOVE icon_enter_more TO wa_saida-observacao.
      MOVE icon_enter_more TO wa_saida2-observacao.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT_ROTEIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA2_NR_SOL  text
*----------------------------------------------------------------------*
FORM read_text_roteiro  USING    p_wa_saida2_nr_rot.

  DATA: wl_name TYPE thead-tdname.

  FREE tg_texto.
  wl_name = p_wa_saida2_nr_rot.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'ZROT'
      language                = sy-langu
      name                    = wl_name
      object                  = 'ZSDROTEIRO'
    TABLES
      lines                   = tg_texto
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_VOLUME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0082_NRO_SOL  text
*----------------------------------------------------------------------*
FORM busca_volume  USING VALUE(p_nro_sol) TYPE zsdt0131-nro_sol
                         VALUE(p_seq)     TYPE zsdt0131-seq
                         VALUE(p_spart)
                   CHANGING wa_0082_qtd_vinc TYPE zsdt0131-qtd_vinc.

  DATA: it_zsdt0131     TYPE STANDARD TABLE OF zsdt0131,
        it_zsdt0138     TYPE STANDARD TABLE OF zsdt0138,
        it_zsdt0082_aux TYPE STANDARD TABLE OF zsdt0082,
        wa_zsdt0131     TYPE zsdt0131,
        wa_zsdt0138     TYPE zsdt0138,
        wa_zsdt0082_aux TYPE zsdt0082,
        vl_total        TYPE zsdt0131-qtd_vinc.

  CLEAR: vl_total, it_zsdt0131, it_zsdt0082_aux, it_zsdt0138.

  CASE p_spart.
    WHEN '04'.

      SELECT *
        FROM zsdt0131
        INTO TABLE it_zsdt0131
        WHERE nro_sol EQ p_nro_sol
          AND status  NE 'X'.

      LOOP AT it_zsdt0131 INTO wa_zsdt0131.
        vl_total = vl_total + wa_zsdt0131-qtd_vinc.
      ENDLOOP.

    WHEN '03'.

      SELECT *
        FROM zsdt0082
        INNER JOIN zsdt0140 ON zsdt0140~nro_sol = zsdt0082~nro_sol AND zsdt0140~seq = zsdt0082~seq
        INTO CORRESPONDING FIELDS OF TABLE it_zsdt0082_aux
        WHERE zsdt0140~nro_sol EQ p_nro_sol
*          AND ZSDT0140~SEQ     EQ P_SEQ.
          AND zsdt0140~status NE 'X'.

      LOOP AT it_zsdt0082_aux INTO wa_zsdt0082_aux.
        vl_total = vl_total + wa_zsdt0082_aux-qte_lib.
      ENDLOOP.

    WHEN '02'.

      SELECT *
        FROM zsdt0138
        INTO TABLE it_zsdt0138
        WHERE nro_sol EQ p_nro_sol
          AND status NE 'X'.

      LOOP AT it_zsdt0138 INTO wa_zsdt0138.
        vl_total = vl_total + wa_zsdt0138-qtd_embarq.
      ENDLOOP.

  ENDCASE.

  wa_0082_qtd_vinc = vl_total.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SALDO_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_SAIDA_QTE_SALDO  text
*----------------------------------------------------------------------*
FORM get_saldo_lote  USING VALUE(p_spart)
                           VALUE(p_sol)
                           CHANGING p_wa_saida_qte_saldo.

  DATA: wa_zsdt0131     TYPE zsdt0131,
        wa_zsdt0082_aux TYPE zsdt0082,
        wa_zsdt0138     TYPE zsdt0138,
        it_zsdt0082_del TYPE STANDARD TABLE OF ty_zsdt0082,
        wa_zsdt0082_del TYPE ty_zsdt0082,
        vl_total        TYPE zsdt0131-qtd_vinc.

  CLEAR: vl_total.

  CASE p_spart.
    WHEN '04'.

      LOOP AT it_zsdt0131 INTO wa_zsdt0131 WHERE vbeln EQ wa_saida-vbeln
                                             AND posnr EQ wa_saida-posnr.
        vl_total = vl_total + wa_zsdt0131-qtd_vinc.
      ENDLOOP.

      p_wa_saida_qte_saldo = p_sol - vl_total.

    WHEN '03'.

      LOOP AT it_zsdt0082_aux INTO wa_zsdt0082_aux WHERE vbeln EQ wa_saida-vbeln
                                                     AND posnr EQ wa_saida-posnr.
        vl_total = vl_total + wa_zsdt0082_aux-qte_lib.
      ENDLOOP.

      p_wa_saida_qte_saldo = p_sol - vl_total.

    WHEN '02'.

*   T_0082          TYPE TABLE OF TY_ZSDT0082,


      it_zsdt0082_del = t_0082.
      DELETE it_zsdt0082_del WHERE ( vbeln NE wa_saida-vbeln OR posnr NE wa_saida-posnr ).
      SORT it_zsdt0082_del BY nro_sol DESCENDING.

      " 12.09.2025 -->
      DELETE it_zsdt0082_del WHERE status EQ '1'.
      "DELETE ADJACENT DUPLICATES FROM it_zsdt0082_del COMPARING nro_sol.
      " 12.09.2025 --<

      LOOP AT it_zsdt0082_del INTO wa_zsdt0082_del.
        LOOP AT it_zsdt0138 INTO wa_zsdt0138
            WHERE nro_sol EQ wa_zsdt0082_del-nro_sol
              AND seq EQ wa_zsdt0082_del-seq. " 12.09.2025

          vl_total = vl_total + wa_zsdt0138-qtd_embarq.
        ENDLOOP.
      ENDLOOP.

      p_wa_saida_qte_saldo = p_sol - vl_total.

  ENDCASE.

ENDFORM.
**<<<------"169500 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_valida_sisdev
*&---------------------------------------------------------------------*
*& Valida cadastro no Sisdev
*&---------------------------------------------------------------------*
*&      --> UV_KUNNR Nº cliente
*&---------------------------------------------------------------------*
FORM zf_valida_sisdev  USING uv_kunnr TYPE kunnr.

  TABLES: kna1.

  "Verificar Região do centro fornecedor.
  SELECT SINGLE regio FROM  t001w
  INTO @DATA(vg_regio) WHERE werks EQ @wa_saida-werks.

  SELECT SINGLE * FROM kna1 WHERE kunnr EQ uv_kunnr.

  IF sy-subrc IS INITIAL AND kna1-regio EQ 'MT' AND vg_regio EQ 'MT'.
    SELECT COUNT(*) FROM zsdt0205
      INTO @DATA(vl_count)
    WHERE cpfcnpj EQ @kna1-stcd1
       OR cpfcnpj EQ @kna1-stcd2.

    IF sy-subrc NE 0.
      SELECT COUNT(*) FROM zsdt0206
      INTO @DATA(vl_count_)
    WHERE cnpj EQ @kna1-stcd1
       OR cnpj EQ @kna1-stcd2.
      IF sy-subrc NE 0.
        MESSAGE 'Cliente não possuem cadastro no SISDEV' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: vg_regio.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_f4_tipo_operacao
*&---------------------------------------------------------------------*
*& Ajuda der pesquisa Tipo de Operação
*&---------------------------------------------------------------------*
*&      --> UE_SAIDA    Área de memória da WA de Saída.
*$      --> E_FIELDNAME Nome do campo da ajuda de pesquisa
*&---------------------------------------------------------------------*
FORM zf_f4_tipo_operacao  USING ue_saida    TYPE any
                                e_fieldname TYPE lvc_fname.

  DATA: tl_return_tab TYPE STANDARD TABLE OF ddshretval,
        tl_fmap       TYPE STANDARD TABLE OF dselc.

  DATA : el_fmap TYPE dselc.

  el_fmap-fldname   = 'DOMVALUE_L'.
  el_fmap-dyfldname = 'Tp. Oper.'.
  APPEND el_fmap TO tl_fmap.
  CLEAR: el_fmap.

  el_fmap-fldname   = 'DDTEXT'.
  el_fmap-dyfldname = 'Descrição'.
  APPEND el_fmap TO tl_fmap.
  CLEAR: el_fmap.

  IF NOT tg_tpopr[] IS INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'DOMVALUE_L'
        window_title    = 'Tipo de Operação'
        value_org       = 'S'
        dynprofield     = 'DOMVALUE_L'
      TABLES
        value_tab       = tg_tpopr
        return_tab      = tl_return_tab
        dynpfld_mapping = tl_fmap
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc IS INITIAL.
      READ TABLE tl_return_tab INTO DATA(el_return_tab) INDEX 1.

      IF sy-subrc IS INITIAL.
* Campo Tipo de Operação (TP_OPER).
        DATA(vl_fieldname)      = e_fieldname.
        DATA(vl_pos)            = strlen( vl_fieldname ) - 2.
        vl_fieldname            = vl_fieldname(vl_pos).
        ue_saida-(vl_fieldname) = el_return_tab-fieldval.
* Campo Descrição Tipo de Operação (TP_OPER_D).
        READ TABLE tg_tpopr INTO DATA(el_tpopr) WITH KEY domvalue_l = el_return_tab-fieldval.
        ue_saida-(e_fieldname) = el_tpopr-ddtext.

      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDIF.

ENDFORM.
**<<<------"169500 - NMS - FIM------>>>
*&---------------------------------------------------------------------*
*& Form f_edit_mask_alv
*&---------------------------------------------------------------------*
FORM f_edit_mask_alv USING uv_fieldname TYPE slis_fieldname.

  READ TABLE estrutura ASSIGNING FIELD-SYMBOL(<fs_estru>)
    WITH KEY fieldname = uv_fieldname.

  IF sy-subrc EQ 0.
    <fs_estru>-ref_fieldname = 'WRBTR'.
    <fs_estru>-ref_tabname = 'BSEG'.
    <fs_estru>-cfieldname = 'BRL'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_collect_alv
*&---------------------------------------------------------------------*
FORM f_collect_alv  USING us_saida TYPE ty_saida.

  DATA ls_temp TYPE ty_saida.

  ls_temp = us_saida.

  LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>)
      WHERE vbeln = us_saida-vbeln
       AND posnr = us_saida-posnr
       AND matnr = us_saida-matnr.

    ADD <fs_saida>-qte_sol TO ls_temp-qte_sol.
    ADD <fs_saida>-vlr_sol TO ls_temp-vlr_sol.
    "ADD <fs_saida>-qte_faturado TO ls_temp-qte_faturado .
    "ADD <fs_saida>-saldo_f_ov TO ls_temp-saldo_f_ov.
    ADD <fs_saida>-saldo_f_sol TO ls_temp-saldo_f_sol.
    ADD <fs_saida>-qte_saldo TO ls_temp-qte_saldo  .
    "ADD <fs_saida>-qte_disponivel TO ls_temp-qte_disponivel.
    ADD <fs_saida>-qte_liberar TO ls_temp-qte_liberar .

  ENDLOOP.

  IF sy-subrc NE 0.

    APPEND INITIAL LINE TO t_saida ASSIGNING <fs_saida>.
  ENDIF.

  MOVE-CORRESPONDING us_saida TO <fs_saida>.

  <fs_saida>-qte_sol = ls_temp-qte_sol.
  <fs_saida>-vlr_sol = ls_temp-vlr_sol.
  "<fs_saida>-qte_faturado = ls_temp-qte_faturado .
  "<fs_saida>-saldo_f_ov = ls_temp-saldo_f_ov.
  <fs_saida>-saldo_f_sol = ls_temp-saldo_f_sol.
  <fs_saida>-qte_saldo = ls_temp-qte_saldo  .
  "<fs_saida>-qte_disponivel = ls_temp-qte_disponivel.
  <fs_saida>-qte_liberar = ls_temp-qte_liberar .

ENDFORM.

*-US191683-25.09.2025-#191683-JT-inicio
************************************************************************************
* valida propriedade INDEA
************************************************************************************
FORM f_valida_propr_indea USING p_vkorg  TYPE vkorg
                                p_kunnr  TYPE kunnr
                                p_werks  TYPE j_1bbranch-branch
                                p_nr_rot TYPE zsdt0132-nr_rot
                       CHANGING p_erro.

  DATA: lr_kunnr    TYPE zde_kunnr_t,
        wr_kunnr    TYPE zde_kunnr_r,
        lc_saida    TYPE zsde0408_t,
        lv_mesg     TYPE string,
        lv_resp     TYPE c,
        lv_rot      TYPE string,
        it_rsparams TYPE TABLE OF rsparams,
        wa_rsparams TYPE rsparams.

  FREE: p_erro, lr_kunnr, it_rsparams.

  CHECK p_nr_rot IS NOT INITIAL AND p_kunnr IS NOT INITIAL AND p_werks IS NOT INITIAL.

*-US192384-03.10.2025-#192384-JT-inicio
  SELECT SINGLE kunnr
    INTO @DATA(_kunnr)
    FROM zsdt0216
   WHERE bukrs           = @p_vkorg
     AND kunnr           = @p_kunnr
     AND setor_atividade = 'A'
     AND revenda         = 'S'.

  CHECK sy-subrc <> 0.
*-US192384-03.10.2025-#192384-JT-fim

  SELECT SINGLE regio
    FROM kna1 INTO @DATA(lva_regio_kunnr)
   WHERE kunnr EQ @p_kunnr.

  CHECK sy-subrc EQ 0 AND lva_regio_kunnr EQ 'MT'.

  SELECT SINGLE regio
    FROM t001w INTO @DATA(lva_regio_werks)
   WHERE werks EQ @p_werks.

  CHECK sy-subrc EQ 0 AND lva_regio_werks EQ 'MT'.

  wr_kunnr-sign    = 'I'.
  wr_kunnr-option  = 'EQ'.
  wr_kunnr-low     = p_kunnr.
  APPEND wr_kunnr TO lr_kunnr.

*--------------------------
*- loop termina qdo cadatrar id propriedade a todas as rotas
*--------------------------
  DO.
    zcl_indea=>selecao_dados_roteiro( EXPORTING i_kunnr           = lr_kunnr
                                                i_nro_rot         = p_nr_rot
                                                i_sem_propriedade = abap_true
                                      IMPORTING e_saida           = lc_saida ).

    IF lc_saida[] IS INITIAL.
*     MESSAGE i024(sd) WITH 'Não foi enontrado Roteiro para o Cliente: ' && p_kunnr DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

    READ TABLE lc_saida INTO DATA(_saida) WITH KEY kunnr = p_kunnr.

    IF sy-subrc = 0 AND _saida-id_propriedade IS INITIAL.
      lv_mesg = 'Roteiro de entrega ' && | { _saida-nr_rot ALPHA = OUT } | &&
                ' não possui um depara com uma Propriedade Indea! Desejar Realizar esse depara agora?'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = lv_mesg
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = abap_off
        IMPORTING
          answer                = lv_resp
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF lv_resp = '2' OR lv_resp = 'A'.
        p_erro = abap_true.
        lv_rot = |{ _saida-nr_rot ALPHA = OUT }|.
        MESSAGE i024(sd) WITH 'Roteiro de entrega  ' lv_rot ' não possui um depara com uma Propriedade Indea!'
                              ' Criação de solicitação não permitida!' DISPLAY LIKE 'I'.
        RETURN.
      ENDIF.

*---------------------------------
*-- cadastro produtor
*---------------------------------
      zcl_indea=>atualizar_tabelas_indea( EXPORTING i_prod     = abap_true
                                                    i_kunnr    = lr_kunnr
                                                    i_no_popup = abap_true
                                          IMPORTING e_erro     = DATA(lv_erro) ).
      IF lv_erro = abap_true.
        p_erro = abap_true.
        RETURN.
      ENDIF.

*---------------------------------
*-- cadastro produtor/propriedade
*---------------------------------
      zcl_indea=>atualizar_tabelas_indea( EXPORTING i_propri   = abap_true
                                                    i_kunnr    = lr_kunnr
                                                    i_no_popup = abap_true
                                          IMPORTING e_erro     = lv_erro ).
      IF lv_erro = abap_true.
        p_erro = abap_true.
        RETURN.
      ENDIF.

*---------------------------------
*-- cadastro de-para INDEA
*---------------------------------
      wa_rsparams-selname = 'P_SUBMIT'.
      wa_rsparams-kind    = 'P'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = abap_true.
      wa_rsparams-high    = abap_false.
      APPEND wa_rsparams TO it_rsparams.

      wa_rsparams-selname = 'P_NR_ROT'.
      wa_rsparams-kind    = 'P'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = p_nr_rot.
      wa_rsparams-high    = p_nr_rot.
      APPEND wa_rsparams TO it_rsparams.

      wa_rsparams-selname = 'P_UR'.
      wa_rsparams-kind    = 'P'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = abap_false.
      wa_rsparams-high    = abap_false.
      APPEND wa_rsparams TO it_rsparams.

      wa_rsparams-selname = 'P_CLI'.
      wa_rsparams-kind    = 'P'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = abap_false.
      wa_rsparams-high    = abap_false.
      APPEND wa_rsparams TO it_rsparams.

      wa_rsparams-selname = 'P_MAPA'.
      wa_rsparams-kind    = 'P'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = abap_false.
      wa_rsparams-high    = abap_false.
      APPEND wa_rsparams TO it_rsparams.

      wa_rsparams-selname = 'P_PROP'.
      wa_rsparams-kind    = 'P'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = abap_true.
      wa_rsparams-high    = abap_false.
      APPEND wa_rsparams TO it_rsparams.

      wa_rsparams-selname = 'P_KUNNR'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = p_kunnr.
      wa_rsparams-high    = abap_off.
      APPEND wa_rsparams TO it_rsparams.

      SUBMIT zsdr020 WITH SELECTION-TABLE it_rsparams AND RETURN.
    ENDIF.
  ENDDO.

ENDFORM.
*-US191683-25.09.2025-#191683-JT-fim

************************************************************************************
************************************************************************************
