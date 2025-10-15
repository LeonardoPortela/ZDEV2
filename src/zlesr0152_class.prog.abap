*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          l_lifnr  TYPE lifnr.


    IF  l_cockpit <> '03'.

      LOOP AT er_data_changed->mt_good_cells INTO ls_good
                               WHERE fieldname = 'AG_FRETE'.
        lv_value = ls_good-value.
        CONDENSE lv_value NO-GAPS.

        READ TABLE t_alv INTO w_alv INDEX ls_good-row_id.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_value
          IMPORTING
            output = l_lifnr.

        IF l_lifnr IS NOT INITIAL.
          SELECT dlgrp, ktokk
            FROM lfa1
            INTO (@DATA(l_dlgrp), @DATA(l_ktokk))  "*-CS2024000522-29.08.2024-JT-#150113-inicio
              UP TO 1 ROWS
           WHERE lifnr = @l_lifnr.
          ENDSELECT.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-111 DISPLAY LIKE 'E'.
            EXIT.
          ELSEIF l_dlgrp <> '0001'.
            MESSAGE s024(sd) WITH TEXT-112 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*-CS2024000522-29.08.2024-JT-#150113-inicio
          IF (  w_alv-tp_frete =  'CIF' AND l_ktokk <> 'ZFIC' ) OR
             (  w_alv-tp_frete <> 'CIF' AND l_ktokk  = 'ZFIC' ).
            MESSAGE s024(sd) WITH 'Agente Frete: ' l_ktokk '...Tipo Frete: ' w_alv-tp_frete DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
*-CS2024000522-29.08.2024-JT-#150113-fim

          LOOP AT t_alv  INTO w_alv WHERE vbeln_venda = w_alv-vbeln_venda
                                      AND nf_venda    = w_alv-nf_venda.
            w_alv-ag_frete  = l_lifnr.
            MODIFY t_alv FROM w_alv INDEX sy-tabix.
          ENDLOOP.

*--------------------------
*------ alterar gente frete
*--------------------------
          zcl_remessa_terceiro=>zif_remessa_terceiro~set_change_agente_frete(
            EXPORTING
              i_remessa_dummy = w_alv-remessa_dummy
              i_ag_frete      = l_lifnr ).
        ENDIF.

      ENDLOOP.

    ELSE.

      LOOP  AT er_data_changed->mt_good_cells INTO ls_good
                                            WHERE fieldname = 'REM_VGBEL' OR  fieldname = 'REM_VGPOS' .
        lv_value = ls_good-value.
        CONDENSE lv_value NO-GAPS.

        READ TABLE t_alv_transf INTO w_alv_transf INDEX ls_good-row_id.

        IF ls_good-fieldname = 'REM_VGBEL'.
          w_alv_transf-rem_vgbel = lv_value.
        ELSE.
          w_alv_transf-rem_vgpos = lv_value.
        ENDIF.

        IF w_alv_transf-rem_vgpos IS INITIAL.
          MESSAGE s024(sd) WITH TEXT-142 DISPLAY LIKE 'E'.
          EXIT.
        ELSE.
          IF w_alv_transf-rem_vgbel IS INITIAL.
            MESSAGE s024(sd) WITH TEXT-142 DISPLAY LIKE 'E'.
            EXIT.
          ELSE.
            SELECT SINGLE *
              FROM ekko INTO @DATA(wl_ekko)
             WHERE ebeln = @w_alv_transf-rem_vgbel
               AND bsart = 'ZUB'.
            IF sy-subrc <> 0.
              MESSAGE s024(sd) WITH TEXT-143 DISPLAY LIKE 'E'.
              EXIT.
            ELSE.
              SELECT SINGLE *
                FROM ekpo INTO @DATA(wl_ekpo)
               WHERE ebeln = @w_alv_transf-rem_vgbel
                 AND ebelp = @w_alv_transf-rem_vgpos.

* (parceiro local entrega)
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wl_ekpo-werks
                IMPORTING
                  output = w_alv_transf-ped_entrega.

              SELECT SINGLE *
                FROM eket INTO @DATA(wl_eket)
               WHERE ebeln = @w_alv_transf-rem_vgbel
                 AND ebelp = @w_alv_transf-rem_vgpos.

              SELECT SINGLE *
                 FROM lips INTO @DATA(wl_lips)
                WHERE vgbel = @wl_ekpo-ebeln.

              IF sy-subrc = 0.
                MESSAGE s024(sd) WITH TEXT-144 DISPLAY LIKE 'E'.
                EXIT.
              ELSE.
                w_alv_transf-ped_menge = wl_ekpo-menge.
                w_alv_transf-ped_meins = wl_ekpo-meins .

                w_alv_transf-rem_matnr = wl_ekpo-matnr.
                w_alv_transf-ped_charg = wl_eket-charg.
                w_alv_transf-ped_lgort = wl_ekpo-lgort.

                MODIFY t_alv_transf FROM w_alv_transf INDEX ls_good-row_id.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.


      LOOP AT er_data_changed->mt_good_cells INTO ls_good
                         WHERE fieldname = 'AG_FRETE'.
        lv_value = ls_good-value.
        CONDENSE lv_value NO-GAPS.

        READ TABLE t_alv_transf INTO w_alv_transf INDEX ls_good-row_id.

        CLEAR: l_lifnr, l_dlgrp.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_value
          IMPORTING
            output = l_lifnr.

        IF l_lifnr IS NOT INITIAL.

          SELECT dlgrp, ktokk
            FROM lfa1
            INTO (@l_dlgrp, @l_ktokk)  "*-CS2024000522-29.08.2024-JT-#150113-inicio
              UP TO 1 ROWS
           WHERE lifnr = @l_lifnr.
          ENDSELECT.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-111 DISPLAY LIKE 'E'.
            EXIT.
          ELSEIF l_dlgrp <> '0001'.
            MESSAGE s024(sd) WITH TEXT-112 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*-CS2024000522-29.08.2024-JT-#150113-inicio
          IF (  w_alv_transf-rem_inco1 =  'CIF' AND l_ktokk <> 'ZFIC' ) OR
             (  w_alv_transf-rem_inco1 <> 'CIF' AND l_ktokk  = 'ZFIC' ).
            MESSAGE s024(sd) WITH 'Agente Frete: ' l_ktokk '...Tipo Frete: ' w_alv_transf-rem_inco1 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
*-CS2024000522-29.08.2024-JT-#150113-fim

          w_alv_transf-ag_frete  = l_lifnr.


          " Busca dados do transporte.
          zcl_frete_remessa_trans=>zif_frete_remessa_trans~get_dados_transporte( EXPORTING i_rem_vbeln     = w_alv_transf-rem_vbeln
                                                                                           i_ebeln         = w_alv_transf-rem_vgbel
                                                                                           i_ebelp         = w_alv_transf-rem_vgpos
                                                                                 IMPORTING e_placa         = w_alv_transf-placa
                                                                                           e_quantidade    = w_alv_transf-quantidade
                                                                                           e_tp_frete      = w_alv_transf-tp_frete
                                                                                           e_itinerario    = w_alv_transf-itinerario
                                                                                           e_vlr_frete     = w_alv_transf-vlr_frete
                                                                                           e_unid_cond     = w_alv_transf-unid_cond
                                                                                           e_lock_ag_frete = l_lock_ag_frete
                                                                                           e_dados_transp  = w_alv_transf-dados_transp
                                                                                 CHANGING  c_ag_frete      = w_alv_transf-ag_frete ).


          MODIFY t_alv_transf  FROM w_alv_transf INDEX ls_good-row_id.

        ENDIF.
      ENDLOOP.

    ENDIF.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD on_hotspot_click.

    DATA: l_ov_dummy      TYPE vbak-vbeln,
          l_remessa_dummy TYPE likp-vbeln,
          l_transp        TYPE vttk-tknum,
          l_doc_custo     TYPE fknum,
          l_ordem_serv    TYPE vbeln_va,
          l_fatura_serv   TYPE vbeln_vf,
          l_dacte         TYPE char10, "j_1bdocnum.
          opt             TYPE ctu_params.   "*-CS2024000522-26.08.2024-JT-#147087-inicio

    CALL METHOD g_grid->check_changed_data.

    IF  l_cockpit <> '03'.

      READ TABLE t_alv INTO w_alv INDEX e_row_id-index.

      CASE e_column_id.

        WHEN 'VBELN_VENDA'.
          SELECT vbeln
            INTO @DATA(l_vbeln_ve)
            FROM vbak
              UP TO 1 ROWS
           WHERE vbeln = @w_alv-vbeln_venda.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'AUN'  FIELD w_alv-vbeln_venda.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'NF_VENDA'.
          SELECT docnum
            INTO @DATA(l_docnum)
            FROM j_1bnfdoc
              UP TO 1 ROWS
           WHERE docnum = @w_alv-docnum.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'JEF'   FIELD w_alv-docnum.
            CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'NF_REMESSA'.
*------------------------------
*------ popup NFs remessa
*------------------------------
          CALL FUNCTION 'ZSD_NOTAS_REMESSA'
            EXPORTING
              i_vbeln          = w_alv-vbeln_venda
              i_chave_nf_venda = w_alv-chave_nfe_venda
              i_docnum         = w_alv-docnum
            IMPORTING
              e_nfnum9         = l_nf_remessa
            EXCEPTIONS
              not_found        = 1
              OTHERS           = 2.

          IF sy-subrc <> 0.
            MESSAGE i024(sd) WITH TEXT-110.
            EXIT.
          ENDIF.

          IF l_nf_remessa IS INITIAL.
            w_alv-nf_remessa = icon_linked_document.
            w_alv-ag_frete   = abap_off.  "*-CS2024000522-18.07.2024-JT-#143588
          ELSE.
*-CS2024000522-18.07.2024-JT-#143588-inicio
            SELECT SINGLE *
              FROM zlest0210
              INTO @DATA(w_0210)
             WHERE chave_nf_venda = @w_alv-chave_nfe_venda.

            IF sy-subrc = 0.
              APPEND w_0210  TO t_zlest0210.
            ENDIF.

            SORT t_zlest0210 BY chave_nf_venda.

            READ TABLE t_zlest0210 INTO w_zlest0210 WITH KEY chave_nf_venda = w_alv-chave_nfe_venda.
            w_alv-nf_remessa    = l_nf_remessa.
            zcl_remessa_terceiro=>zif_remessa_terceiro~get_agente_frete( EXPORTING i_chave_nfe     = w_zlest0210-chave_nf_cta_ordem
                                                                                   i_vbeln         = w_alv-vbeln_venda
                                                                          CHANGING e_agente_frete  = w_alv-ag_frete ).
*-CS2024000522-18.07.2024-JT-#143588-fim
          ENDIF.

          MODIFY t_alv FROM w_alv INDEX e_row_id-index..

        WHEN 'OV_DUMMY'.
*------------------------------
*------ GErar ov DUMMY
*------------------------------
          SELECT vbeln
            INTO @DATA(l_vbeln)
            FROM vbak
              UP TO 1 ROWS
           WHERE vbeln = @w_alv-ov_dummy.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'AUN'  FIELD w_alv-ov_dummy.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
          ELSE.
            IF w_alv-nf_remessa = icon_linked_document.
              MESSAGE s024(sd) WITH TEXT-120 DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.

            PERFORM f_gera_ov_dummy    USING w_alv-vbeln_venda
                                             w_alv-chave_nfe_venda
                                             w_alv-nf_venda
                                    CHANGING l_ov_dummy.

            LOOP AT t_alv  INTO w_alv WHERE vbeln_venda = w_alv-vbeln_venda.
              w_alv-ov_dummy  = l_ov_dummy.
              MODIFY t_alv FROM w_alv INDEX sy-tabix.
            ENDLOOP.
          ENDIF.

        WHEN 'REMESSA_DUMMY'.
*------------------------------
*------ checa ov DUMMY
*------------------------------
          SELECT vbeln
            INTO l_vbeln
            FROM vbak
              UP TO 1 ROWS
           WHERE vbeln = w_alv-ov_dummy.
          ENDSELECT.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-131 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*------------------------------
*------ GErar remessa DUMMY
*------------------------------
          SELECT vbeln
            INTO l_vbeln
            FROM likp
              UP TO 1 ROWS
           WHERE vbeln = w_alv-remessa_dummy.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'VL'    FIELD w_alv-remessa_dummy.
            CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
          ELSE.
            IF w_alv-nf_remessa = icon_linked_document.
              MESSAGE s024(sd) WITH TEXT-120 DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.

            PERFORM f_gera_remessa_dummy USING w_alv-vbeln_venda
                                               w_alv-chave_nfe_venda
                                               w_alv-ov_dummy
                                               w_alv-nf_venda
                                               w_alv-docnum
                                      CHANGING l_remessa_dummy.

            LOOP AT t_alv  INTO w_alv WHERE vbeln_venda = w_alv-vbeln_venda
                                        AND nf_venda    = w_alv-nf_venda.
              w_alv-remessa_dummy  = l_remessa_dummy.
              MODIFY t_alv FROM w_alv INDEX sy-tabix.
            ENDLOOP.
          ENDIF.

        WHEN 'DADOS_TRANSP'.
          IF     w_alv-dados_transp = icon_icon_list.   "*-CS2024000522-29.08.2024-JT-#150113-inicio
            EXIT.                                       "*-CS2024000522-29.08.2024-JT-#150113-inicio
          ELSEIF w_alv-nf_remessa   = icon_linked_document.
            MESSAGE s024(sd) WITH TEXT-130 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          SELECT vbeln
            INTO l_vbeln
            FROM vbak
              UP TO 1 ROWS
           WHERE vbeln = w_alv-ov_dummy.
          ENDSELECT.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-131 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          SELECT vbeln
            INTO l_vbeln
            FROM likp
              UP TO 1 ROWS
           WHERE vbeln = w_alv-remessa_dummy.
          ENDSELECT.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-132 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          SELECT dlgrp, ktokk
            FROM lfa1
            INTO (@DATA(l_dlgrp), @DATA(l_ktokk))  "*-CS2024000522-29.08.2024-JT-#150113-inicio
              UP TO 1 ROWS
           WHERE lifnr = @w_alv-ag_frete.
          ENDSELECT.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-111 DISPLAY LIKE 'E'.
            EXIT.
          ELSEIF l_dlgrp <> '0001'.
            MESSAGE s024(sd) WITH TEXT-112 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*-CS2024000522-29.08.2024-JT-#150113-inicio
          IF (  w_alv-tp_frete =  'CIF' AND l_ktokk <> 'ZFIC' ) OR
             (  w_alv-tp_frete <> 'CIF' AND l_ktokk  = 'ZFIC' ).
            MESSAGE s024(sd) WITH 'Agente Frete: ' l_ktokk '...Tipo Frete: ' w_alv-tp_frete DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
*-CS2024000522-29.08.2024-JT-#150113-fim

*------------------------------
*------ popup dados transporte
*------------------------------
          CALL FUNCTION 'ZSD_DADOS_TRANSPORTE'
            EXPORTING
              i_vbeln_venda    = w_alv-vbeln_venda
              i_ov_dummy       = w_alv-ov_dummy
              i_remessa_dummy  = w_alv-remessa_dummy
              i_refkey         = w_alv-refkey
              i_ag_frete       = w_alv-ag_frete
              i_nf_remessa     = w_alv-nf_remessa
              i_nf_venda       = w_alv-nf_venda
              i_chave_nf_venda = w_alv-chave_nfe_venda. "*-CS2024000522-18.07.2024-JT-#143588

*------------------------------
*------ refresh alv
*------------------------------
          LOOP AT t_alv  INTO w_alv WHERE vbeln_venda = w_alv-vbeln_venda
                                      AND nf_venda    = w_alv-nf_venda.
            l_tabix = sy-tabix.

            zcl_remessa_terceiro=>zif_remessa_terceiro~get_dados_transporte( EXPORTING i_ov_dummy      = w_alv-ov_dummy
                                                                                       i_remessa_dummy = w_alv-remessa_dummy
                                                                             IMPORTING e_placa         = w_alv-placa
                                                                                       e_quantidade    = w_alv-quantidade
                                                                                       e_tp_frete      = w_alv-tp_frete
                                                                                       e_itinerario    = w_alv-itinerario
                                                                                       e_vlr_frete     = w_alv-vlr_frete
                                                                                       e_unid_cond     = w_alv-unid_cond
                                                                                       e_lock_ag_frete = l_lock_ag_frete
                                                                                       e_dados_transp  = w_alv-dados_transp
                                                                             CHANGING  c_ag_frete      = w_alv-ag_frete ).

            IF l_lock_ag_frete = abap_true.
              FREE: t_style.
              w_style-fieldname  = 'AG_FRETE'.
              w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
              APPEND w_style    TO t_style.
              w_alv-cellstyles[] = t_style[].
            ELSE.
              FREE: t_style.
              w_style-fieldname  = 'AG_FRETE'.
              w_style-style      = cl_gui_alv_grid=>mc_style_enabled.
              APPEND w_style    TO t_style.
              w_alv-cellstyles[] = t_style[].
            ENDIF.

            MODIFY t_alv FROM w_alv INDEX l_tabix.
          ENDLOOP.

        WHEN 'TRANSP'.
*------------------------------
*------ GErar doc transporte
*------------------------------
          SELECT tknum
            INTO @DATA(l_tknum)
            FROM vttk
              UP TO 1 ROWS
           WHERE tknum = @w_alv-transp.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'TNR' FIELD w_alv-transp.
            CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
          ELSE.
            SELECT vbeln
              INTO l_vbeln
              FROM likp
                UP TO 1 ROWS
             WHERE vbeln = w_alv-remessa_dummy.
            ENDSELECT.

            IF sy-subrc <> 0.
              MESSAGE s024(sd) WITH TEXT-132 DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.

*--------------------------
*-------- gera doc transporte
*--------------------------
            PERFORM f_gera_transporte    USING w_alv-vbeln_venda
                                               w_alv-ov_dummy
                                               w_alv-remessa_dummy
                                               w_alv-nf_venda
                                               w_alv-vlr_frete        "*-CS2024000522-18.07.2024-JT-#143588
                                               w_alv-chave_nfe_venda  "*-CS2024000522-18.07.2024-JT-#143588
                                      CHANGING l_transp.

            LOOP AT t_alv  INTO w_alv WHERE vbeln_venda = w_alv-vbeln_venda
                                        AND nf_venda    = w_alv-nf_venda.
              w_alv-transp    = l_transp.
              MODIFY t_alv FROM w_alv INDEX sy-tabix.
            ENDLOOP.

*--------------------------
*-------- gera doc custo
*--------------------------
            IF l_transp IS NOT INITIAL AND l_transp(1) <> '@'.
              PERFORM f_gera_doc_custo     USING w_alv-transp
                                                 w_alv-vbeln_venda
                                                 w_alv-nf_venda
                                                 w_alv-remessa_dummy
                                                 w_alv-ag_frete
                                        CHANGING l_doc_custo
                                                 l_ordem_serv
                                                 l_fatura_serv
                                                 l_dacte.

              LOOP AT t_alv  INTO w_alv WHERE vbeln_venda = w_alv-vbeln_venda
                                          AND nf_venda    = w_alv-nf_venda.
                w_alv-doc_custo = l_doc_custo.
                w_alv-ov_serv   = l_ordem_serv.
                w_alv-fat_serv  = l_fatura_serv.
                w_alv-dacte     = l_dacte.
                MODIFY t_alv FROM w_alv INDEX sy-tabix.
              ENDLOOP.
              PERFORM f_processa_dados.
            ENDIF.
          ENDIF.

        WHEN 'DOC_CUSTO'.
*------------------------------
*------ Consulta doc Custo
*------------------------------
          SELECT fknum
            INTO @DATA(l_fknum)
            FROM vfkk
              UP TO 1 ROWS
           WHERE fknum = @w_alv-doc_custo.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'FKK'  FIELD w_alv-doc_custo.
            CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
          ELSE.
            SELECT tknum
              INTO l_tknum
              FROM vttk
                UP TO 1 ROWS
             WHERE tknum = w_alv-transp.
            ENDSELECT.

            IF sy-subrc = 0.
              PERFORM f_gera_doc_custo     USING w_alv-transp
                                                 w_alv-vbeln_venda
                                                 w_alv-nf_venda
                                                 w_alv-remessa_dummy
                                                 w_alv-ag_frete
                                        CHANGING l_doc_custo
                                                 l_ordem_serv
                                                 l_fatura_serv
                                                 l_dacte.

              LOOP AT t_alv  INTO w_alv WHERE vbeln_venda = w_alv-vbeln_venda
                                          AND nf_venda    = w_alv-nf_venda.
                w_alv-doc_custo = l_doc_custo.
                w_alv-ov_serv   = l_ordem_serv.
                w_alv-fat_serv  = l_fatura_serv.
                w_alv-dacte     = l_dacte.
                MODIFY t_alv FROM w_alv INDEX sy-tabix.
              ENDLOOP.
              PERFORM f_processa_dados.
            ENDIF.
          ENDIF.

        WHEN 'OV_SERV'.
*------------------------------
*------ Consulta OV serv
*------------------------------
          SELECT vbeln
            INTO @DATA(l_vbeln3)
            FROM vbak
              UP TO 1 ROWS
           WHERE vbeln = @w_alv-ov_serv.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'AUN'  FIELD w_alv-ov_serv.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
          ELSE.
            SELECT tknum
              INTO l_tknum
              FROM vttk
                UP TO 1 ROWS
             WHERE tknum = w_alv-transp.
            ENDSELECT.

            IF sy-subrc = 0.
              PERFORM f_gera_doc_custo     USING w_alv-transp
                                                 w_alv-vbeln_venda
                                                 w_alv-nf_venda
                                                 w_alv-remessa_dummy
                                                 w_alv-ag_frete
                                        CHANGING l_doc_custo
                                                 l_ordem_serv
                                                 l_fatura_serv
                                                 l_dacte.

              LOOP AT t_alv  INTO w_alv WHERE vbeln_venda = w_alv-vbeln_venda
                                          AND nf_venda    = w_alv-nf_venda.
                w_alv-doc_custo = l_doc_custo.
                w_alv-ov_serv   = l_ordem_serv.
                w_alv-fat_serv  = l_fatura_serv.
                w_alv-dacte     = l_dacte.
                MODIFY t_alv FROM w_alv INDEX sy-tabix.
              ENDLOOP.
              PERFORM f_processa_dados.
            ENDIF.
          ENDIF.

        WHEN 'FAT_SERV'.
*------------------------------
*------ Consulta Fatura serv
*------------------------------
          SELECT vbeln
            INTO @DATA(l_vbeln4)
            FROM vbrk
              UP TO 1 ROWS
           WHERE vbeln = @w_alv-fat_serv.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'VF'   FIELD w_alv-fat_serv.
            CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
          ELSE.
            SELECT tknum
              INTO l_tknum
              FROM vttk
                UP TO 1 ROWS
             WHERE tknum = w_alv-transp.
            ENDSELECT.

            IF sy-subrc = 0.
              PERFORM f_gera_doc_custo     USING w_alv-transp
                                                 w_alv-vbeln_venda
                                                 w_alv-nf_venda
                                                 w_alv-remessa_dummy
                                                 w_alv-ag_frete
                                        CHANGING l_doc_custo
                                                 l_ordem_serv
                                                 l_fatura_serv
                                                 l_dacte.

              LOOP AT t_alv  INTO w_alv WHERE vbeln_venda = w_alv-vbeln_venda
                                          AND nf_venda    = w_alv-nf_venda.
                w_alv-doc_custo = l_doc_custo.
                w_alv-ov_serv   = l_ordem_serv.
                w_alv-fat_serv  = l_fatura_serv.
                w_alv-dacte     = l_dacte.
                MODIFY t_alv FROM w_alv INDEX sy-tabix.
              ENDLOOP.
              PERFORM f_processa_dados.
            ENDIF.
          ENDIF.

        WHEN 'DACTE'.
*------------------------------
*------ Consulta Fatura serv
*------------------------------
          IF w_alv-dacte(1) <> '@'.
            SELECT bukrs, docnum
              INTO @DATA(w_doc)
              FROM j_1bnfdoc
                UP TO 1 ROWS
             WHERE docnum = @w_alv-dacte.
            ENDSELECT.

            IF sy-subrc = 0.
              SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD w_doc-docnum.
              SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD w_doc-bukrs.
              CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
            ENDIF.
          ENDIF.

      ENDCASE.
    ELSE.
** US - 92467 - Inicio - CBRAND
      READ TABLE t_alv_transf INTO w_alv_transf INDEX e_row_id-index.

      CASE e_column_id.
        WHEN 'REM_FAT'.
          IF w_alv_transf-rem_fat IS NOT INITIAL.
*-CS2024000522-26.08.2024-JT-#147087-inicio
            FREE tl_bdc.
            PERFORM f_preencher_dynpro USING:
                       'X' 'SAPLMIGO'            '0001',
                       ' ' 'BDC_OKCODE'          'OK_GO',
                       ' ' 'BDC_SUBSCR'          'SAPLMIGO',
                       ' ' 'GODYNPRO-ACTION'     'A04',
                       ' ' 'GODYNPRO-REFDOC'     'R02',
                       ' ' 'BDC_SUBSCR'          'SAPLMIGO',
                       ' ' 'BDC_CURSOR'          'GODYNPRO-MAT_DOC',
                       ' ' 'GODYNPRO-MAT_DOC'    w_alv_transf-rem_fat,
                       ' ' 'GODYNPRO-DOC_YEAR'   w_alv_transf-rem_fat_ano,
                       ' ' 'BDC_SUBSCR'          'SAPLMIGO'.

            opt-dismode = 'E'.
            opt-defsize = ' '.
            CALL TRANSACTION 'MIGO' USING tl_bdc OPTIONS FROM opt.
*           SET PARAMETER ID 'MBN' FIELD w_alv_transf-rem_fat.
*           SET PARAMETER ID 'MJA' FIELD w_alv_transf-rem_fat_ano.
*           CALL TRANSACTION 'MIGO_GO' AND SKIP FIRST SCREEN.
*-CS2024000522-26.08.2024-JT-#147087-fim
          ENDIF.
        WHEN 'REM_VBELN'.

          SELECT vbeln
            INTO l_vbeln
            FROM likp
              UP TO 1 ROWS
           WHERE vbeln = w_alv_transf-rem_vbeln.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'VL'    FIELD w_alv_transf-rem_vbeln.
            CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
          ELSE.

            vg_ebeln = w_alv_transf-rem_vgbel.
            vg_ebelp = w_alv_transf-rem_vgpos.
            vg_bsart = 'ZUB'.

            CALL FUNCTION 'ZMM_DADOS_PEDIDO'
              EXPORTING
                i_ebeln         = vg_ebeln
                i_ebelp         = vg_ebelp
                i_bsart         = vg_bsart
              TABLES
                t_dados_remessa = t_dados_remessa.

*-CS2024000522-26.08.2024-JT-#147087-inicio
            READ TABLE t_dados_remessa INTO DATA(w_dados_remessa) INDEX 1.
            IF sy-subrc = 0.
              MESSAGE i024(sd) WITH 'Rememessa Gerada: ' w_dados_remessa-rem_vbeln.
            ENDIF.
*-CS2024000522-26.08.2024-JT-#147087-fim

*-CS2024000522-26.08.2024-JT-#147087-inicio
*            LOOP AT t_dados_remessa INTO DATA(wa_dados_remessa).
*              CLEAR: w_alv_transf.
*              w_alv_transf-rem_vbeln   = wa_dados_remessa-rem_vbeln.
*              w_alv_transf-rem_brgew   = wa_dados_remessa-rem_brgew.
*              w_alv_transf-transp      = icon_execute_object.
*              w_alv_transf-doc_custo   = icon_icon_list.
*              w_alv_transf-ov_serv     = icon_icon_list.
*              w_alv_transf-fat_serv    = icon_icon_list.
*              w_alv_transf-dacte       = icon_icon_list.
*              CLEAR:t_style, w_style.
*              w_style-fieldname  = 'AG_FRETE'.
*              w_style-style      = cl_gui_alv_grid=>mc_style_enabled.
*              INSERT w_style    INTO TABLE t_style.
*              w_alv_transf-cellstyles[] = t_style[].
*              APPEND w_alv_transf TO  t_alv_transf.
*            ENDLOOP.
*-CS2024000522-26.08.2024-JT-#147087-fim
            "
            PERFORM f_selecao_dados_nf_propria.
            PERFORM f_processa_dados.
            PERFORM f_alv_saida.

            SORT t_alv_transf BY ped_ebeln ASCENDING.

            "PERFORM f_ztransf USING e_row_id-index. "Comentado - DEVK9A1UVH - 02.02.2024 - #84717 RSA
          ENDIF.

          "Início - DEVK9A1UVH - 02.02.2024 - #84717 RSA
        WHEN 'EBELN_IC'.

          IF NOT w_alv_transf-rem_vbeln IS INITIAL.

            CALL FUNCTION 'ZMM_DADOS_PEDIDO'
              EXPORTING
                i_vbeln = w_alv_transf-rem_vbeln.
          ENDIF.
          "Fim - DEVK9A1UVH - 02.02.2024 - #84717 RSA


        WHEN 'DADOS_TRANSP'.
          IF     w_alv_transf-dados_transp = icon_icon_list.   "*-CS2024000522-29.08.2024-JT-#150113-inicio
            EXIT.                                              "*-CS2024000522-29.08.2024-JT-#150113-inicio
          ELSEIF w_alv_transf-rem_vbeln    = icon_linked_document.
            MESSAGE s024(sd) WITH TEXT-146 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          SELECT vbeln
            INTO l_vbeln
            FROM likp
              UP TO 1 ROWS
           WHERE vbeln = w_alv_transf-rem_vbeln..
          ENDSELECT.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-132 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          CLEAR: l_dlgrp, l_ktokk.
          SELECT dlgrp, ktokk
            INTO (@l_dlgrp, @l_ktokk)  "*-CS2024000522-29.08.2024-JT-#150113-inicio
            FROM lfa1
              UP TO 1 ROWS
           WHERE lifnr = @w_alv_transf-ag_frete.
          ENDSELECT.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-111 DISPLAY LIKE 'E'.
            EXIT.
          ELSEIF l_dlgrp <> '0001'.
            MESSAGE s024(sd) WITH TEXT-112 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*-CS2024000522-29.08.2024-JT-#150113-inicio
          IF (  w_alv_transf-rem_inco1 =  'CIF' AND l_ktokk <> 'ZFIC' ) OR
             (  w_alv_transf-rem_inco1 <> 'CIF' AND l_ktokk  = 'ZFIC' ).
            MESSAGE s024(sd) WITH 'Agente Frete: ' l_ktokk '...Tipo Frete: ' w_alv_transf-rem_inco1 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
*-CS2024000522-29.08.2024-JT-#150113-fim

*------------------------------
*------ popup dados transporte
*------------------------------
          CALL FUNCTION 'ZSD_DADOS_TRANSPORTE'
            EXPORTING
              i_rem_vbeln = w_alv_transf-rem_vbeln
              i_rem_posnr = w_alv_transf-rem_posnr
              i_ag_frete  = w_alv_transf-ag_frete
              i_ped_bsart = w_alv_transf-ped_bsart
              i_tipo      = 'T'.

*------------------------------
*------ refresh alv
*------------------------------
          LOOP AT t_alv_transf  INTO w_alv_transf WHERE rem_vbeln  = w_alv_transf-rem_vbeln
                                                    AND rem_posnr  = w_alv_transf-rem_posnr
                                                    AND rem_vgbel  = w_alv_transf-rem_vgbel
                                                    AND rem_vgpos  = w_alv_transf-rem_vgpos.
            l_tabix = sy-tabix.


            zcl_frete_remessa_trans=>zif_frete_remessa_trans~get_dados_transporte( EXPORTING i_rem_vbeln     = w_alv_transf-rem_vbeln
                                                                                             i_ebeln         = w_alv_transf-rem_vgbel
                                                                                             i_ebelp         = w_alv_transf-rem_vgpos
                                                                                   IMPORTING e_placa         = w_alv_transf-placa
                                                                                             e_quantidade    = w_alv_transf-quantidade
                                                                                             e_tp_frete      = w_alv_transf-tp_frete
                                                                                             e_itinerario    = w_alv_transf-itinerario
                                                                                             e_vlr_frete     = w_alv_transf-vlr_frete
                                                                                             e_unid_cond     = w_alv_transf-unid_cond
                                                                                             e_lock_ag_frete = l_lock_ag_frete
                                                                                             e_dados_transp  = w_alv_transf-dados_transp
                                                                                   CHANGING  c_ag_frete      = w_alv_transf-ag_frete ).

            IF l_lock_ag_frete = abap_true.
              FREE: t_style.
              w_style-fieldname  = 'AG_FRETE'.
              w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
              INSERT w_style    INTO TABLE t_style.


              CLEAR: w_style.
              w_style-fieldname  = 'REM_VGBEL'.
              w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
              INSERT w_style    INTO TABLE t_style.

              CLEAR: w_style.
              w_style-fieldname  = 'REM_VGPOS'.
              w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
              INSERT w_style    INTO TABLE t_style.

              w_alv_transf-cellstyles[] = t_style[].

            ELSE.
              FREE: t_style.
              w_style-fieldname  = 'AG_FRETE'.
              w_style-style      = cl_gui_alv_grid=>mc_style_enabled.
              INSERT w_style    INTO TABLE t_style.


              CLEAR: w_style.
              w_style-fieldname  = 'REM_VGBEL'.
              w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
              INSERT w_style    INTO TABLE t_style.

              CLEAR: w_style.
              w_style-fieldname  = 'REM_VGPOS'.
              w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
              INSERT w_style    INTO TABLE t_style.

              w_alv_transf-cellstyles[] = t_style[].



            ENDIF.

            MODIFY t_alv_transf FROM w_alv_transf INDEX l_tabix.
          ENDLOOP.

        WHEN 'TRANSP'.
*------------------------------
*------ Gerar doc transporte
*------------------------------
          CLEAR: l_tknum.
          SELECT tknum
            INTO l_tknum
            FROM vttk
              UP TO 1 ROWS
           WHERE tknum = w_alv_transf-transp.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'TNR' FIELD w_alv_transf-transp.
            CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
          ELSE.
            SELECT vbeln
              INTO l_vbeln
              FROM likp
                UP TO 1 ROWS
             WHERE vbeln = w_alv_transf-rem_vbeln.
            ENDSELECT.

            IF sy-subrc <> 0.
              MESSAGE s024(sd) WITH TEXT-132 DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.

*--------------------------
*-------- gera doc transporte
*--------------------------
            IF w_alv_transf-transp <> icon_message_error.
              PERFORM f_gera_transporte_transf USING  w_alv_transf CHANGING l_transp.

              IF l_transp IS NOT INITIAL AND l_transp(1) <> '@'.
                LOOP AT t_alv_transf  INTO w_alv_transf WHERE rem_vbeln  = w_alv_transf-rem_vbeln
                                                          AND rem_posnr  = w_alv_transf-rem_posnr
                                                          AND rem_vgbel  = w_alv_transf-rem_vgbel
                                                          AND rem_vgpos  = w_alv_transf-rem_vgpos.

                  w_alv_transf-transp   = l_transp.
                  MODIFY t_alv_transf FROM w_alv_transf INDEX sy-tabix.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.

*--------------------------
*-------- gera doc custo
*--------------------------
          IF l_transp IS NOT INITIAL AND l_transp(1) <> '@'.
            PERFORM f_gera_doc_custo_transf  USING  w_alv_transf
                                              CHANGING l_doc_custo
                                                       l_ordem_serv
                                                       l_fatura_serv
                                                       l_dacte.

            LOOP AT t_alv_transf  INTO w_alv_transf WHERE rem_vbeln  = w_alv_transf-rem_vbeln
                                                      AND rem_posnr  = w_alv_transf-rem_posnr
                                                      AND rem_vgbel  = w_alv_transf-rem_vgbel
                                                      AND rem_vgpos  = w_alv_transf-rem_vgpos.
              w_alv_transf-doc_custo = l_doc_custo.
              w_alv_transf-ov_serv   = l_ordem_serv.
              w_alv_transf-fat_serv  = l_fatura_serv.
              w_alv_transf-dacte     = l_dacte.

              MODIFY t_alv_transf FROM w_alv_transf INDEX sy-tabix.
            ENDLOOP.
            PERFORM f_processa_dados.
          ENDIF.

        WHEN 'DOC_CUSTO'.
*------------------------------
*------ Consulta doc Custo
*------------------------------
          CLEAR: l_fknum.
          SELECT fknum
            INTO l_fknum
            FROM vfkk
              UP TO 1 ROWS
           WHERE fknum = w_alv_transf-doc_custo.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'FKK'  FIELD w_alv_transf-doc_custo.
            CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
          ELSE.
            SELECT tknum
              INTO l_tknum
              FROM vttk
                UP TO 1 ROWS
             WHERE tknum = w_alv_transf-transp.
            ENDSELECT.

            IF sy-subrc = 0.
              PERFORM f_gera_doc_custo_transf  USING  w_alv_transf
                                                CHANGING l_doc_custo
                                                         l_ordem_serv
                                                         l_fatura_serv
                                                         l_dacte.

              LOOP AT t_alv_transf  INTO w_alv_transf WHERE rem_vbeln  = w_alv_transf-rem_vbeln
                                                        AND rem_posnr  = w_alv_transf-rem_posnr
                                                        AND rem_vgbel  = w_alv_transf-rem_vgbel
                                                        AND rem_vgpos  = w_alv_transf-rem_vgpos.
                w_alv_transf-doc_custo = l_doc_custo.
                w_alv_transf-ov_serv   = l_ordem_serv.
                w_alv_transf-fat_serv  = l_fatura_serv.
                w_alv_transf-dacte     = l_dacte.

                MODIFY t_alv_transf FROM w_alv_transf INDEX sy-tabix.
              ENDLOOP.
              PERFORM f_processa_dados.
            ENDIF.
          ENDIF.

        WHEN 'OV_SERV'.
*------------------------------
*------ Consulta OV serv
*------------------------------
          CLEAR: l_vbeln3.
          SELECT vbeln
            INTO l_vbeln3
            FROM vbak
              UP TO 1 ROWS
           WHERE vbeln = w_alv_transf-ov_serv.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'AUN'  FIELD w_alv_transf-ov_serv.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
          ELSE.
            SELECT tknum
              INTO l_tknum
              FROM vttk
                UP TO 1 ROWS
             WHERE tknum = w_alv_transf-transp.
            ENDSELECT.

            IF sy-subrc = 0.
              PERFORM f_gera_doc_custo_transf  USING  w_alv_transf
                                                CHANGING l_doc_custo
                                                         l_ordem_serv
                                                         l_fatura_serv
                                                         l_dacte.

              LOOP AT t_alv_transf  INTO w_alv_transf WHERE rem_vbeln  = w_alv_transf-rem_vbeln
                                                        AND rem_posnr  = w_alv_transf-rem_posnr
                                                        AND rem_vgbel  = w_alv_transf-rem_vgbel
                                                        AND rem_vgpos  = w_alv_transf-rem_vgpos.
                w_alv_transf-doc_custo = l_doc_custo.
                w_alv_transf-ov_serv   = l_ordem_serv.
                w_alv_transf-fat_serv  = l_fatura_serv.
                w_alv_transf-dacte     = l_dacte.

                MODIFY t_alv_transf FROM w_alv_transf INDEX sy-tabix.
              ENDLOOP.
              PERFORM f_processa_dados.
            ENDIF.
          ENDIF.


        WHEN 'FAT_SERV'.
*------------------------------
*------ Consulta Fatura serv
*------------------------------
          CLEAR: l_vbeln4.
          SELECT vbeln
            INTO l_vbeln4
            FROM vbrk
              UP TO 1 ROWS
           WHERE vbeln = w_alv_transf-fat_serv.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'VF'   FIELD w_alv_transf-fat_serv.
            CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
          ELSE.
*--------------------------
*-------- gera doc custo
*--------------------------
            IF l_transp IS NOT INITIAL AND l_transp(1) <> '@'.
              PERFORM f_gera_doc_custo_transf  USING  w_alv_transf
                                                CHANGING l_doc_custo
                                                         l_ordem_serv
                                                         l_fatura_serv
                                                         l_dacte.

              LOOP AT t_alv_transf  INTO w_alv_transf WHERE rem_vbeln  = w_alv_transf-rem_vbeln
                                                        AND rem_posnr  = w_alv_transf-rem_posnr
                                                        AND rem_vgbel  = w_alv_transf-rem_vgbel
                                                        AND rem_vgpos  = w_alv_transf-rem_vgpos.
                w_alv_transf-doc_custo = l_doc_custo.
                w_alv_transf-ov_serv   = l_ordem_serv.
                w_alv_transf-fat_serv  = l_fatura_serv.
                w_alv_transf-dacte     = l_dacte.

                MODIFY t_alv_transf FROM w_alv_transf INDEX sy-tabix.
              ENDLOOP.
              PERFORM f_processa_dados.
            ENDIF.
          ENDIF.

        WHEN 'DACTE'.
          CLEAR: w_doc.
          IF w_alv_transf-dacte(1) <> '@'.
            SELECT bukrs, docnum
              INTO @DATA(w_doc_transf)
              FROM j_1bnfdoc
                UP TO 1 ROWS
             WHERE docnum = @w_alv_transf-dacte.
            ENDSELECT.

            IF sy-subrc = 0.
              SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD w_doc_transf-docnum.
              SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD w_doc_transf-bukrs.
              CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
            ENDIF.
          ENDIF.

        WHEN 'REM_DOC_NF'.
          SELECT  bukrs, docnum
            INTO @DATA(w_doc_danfe)
            FROM j_1bnfdoc
              UP TO 1 ROWS
            WHERE docnum = @w_alv_transf-rem_doc_nf.
          ENDSELECT.

          IF sy-subrc = 0.
            SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD w_doc_danfe-docnum.
            SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD w_doc_danfe-bukrs.
            CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.

    ENDIF.

    w_stable-row = abap_true.
    w_stable-col = abap_true.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.
  METHOD handle_toolbar.
    PERFORM f_handle_toolbar USING e_object .
  ENDMETHOD .
  METHOD handle_user_command.
    DATA:  l_estorno(1)        TYPE c.
    DATA: tl_index_rows TYPE lvc_t_row,
          wl_index_rows TYPE lvc_s_row.

    CASE e_ucomm.
      WHEN 'GER_REM'.
        CLEAR: w_alv_transf.
*-CS2024000522-26.08.2024-JT-#147087-inicio
*        w_alv_transf-rem_vbeln   = icon_linked_document.
*        w_alv_transf-transp      = icon_execute_object.
*        w_alv_transf-doc_custo   = icon_icon_list.
*        w_alv_transf-ov_serv     = icon_icon_list.
*        w_alv_transf-fat_serv    = icon_icon_list.
*        w_alv_transf-dacte       = icon_icon_list.
*
*        CLEAR:t_style, w_style.
*        w_style-fieldname  = 'AG_FRETE'.
*        w_style-style      = cl_gui_alv_grid=>mc_style_enabled.
*        INSERT w_style    INTO TABLE t_style.
*
*        w_alv_transf-cellstyles[] = t_style[].
*
*        APPEND w_alv_transf TO  t_alv_transf.
*-CS2024000522-26.08.2024-JT-#147087-fim
        "INSERT  w_alv_transf INTO t_alv_transf  INDEX 1.

*-CS2024000522-18.07.2024-JT-#147087-inicio
        vg_ebeln = w_alv_transf-rem_vgbel.
        vg_ebelp = w_alv_transf-rem_vgpos.
        vg_bsart = 'ZUB'.

        CALL FUNCTION 'ZMM_DADOS_PEDIDO'
          EXPORTING
            i_ebeln         = vg_ebeln
            i_ebelp         = vg_ebelp
            i_bsart         = vg_bsart
          TABLES
            t_dados_remessa = t_dados_remessa.

*-CS2024000522-26.08.2024-JT-#147087-inicio
*       LOOP AT t_dados_remessa INTO DATA(w_dados_remessa).
*         MESSAGE i024(sd) WITH 'Rememessa Gerada: ' w_dados_remessa-rem_vbeln.
*       ENDLOOP.
        PERFORM f_exibir_remessas.
        PERFORM f_selecao_dados_nf_propria. "*-CS2024000522-18.07.2024-JT-#143588
        PERFORM f_processa_dados.
*-CS2024000522-26.08.2024-JT-#147087-fim

*        LOOP AT t_dados_remessa INTO DATA(wa_dados_remessa).
*          CLEAR: w_alv_transf.
*          w_alv_transf-rem_vbeln   = wa_dados_remessa-rem_vbeln.
*          w_alv_transf-rem_brgew   = wa_dados_remessa-rem_brgew.
*          w_alv_transf-transp      = icon_execute_object.
*          w_alv_transf-doc_custo   = icon_icon_list.
*          w_alv_transf-ov_serv     = icon_icon_list.
*          w_alv_transf-fat_serv    = icon_icon_list.
*          w_alv_transf-dacte       = icon_icon_list.
*          CLEAR:t_style, w_style.
*          w_style-fieldname  = 'AG_FRETE'.
*          w_style-style      = cl_gui_alv_grid=>mc_style_enabled.
*          INSERT w_style    INTO TABLE t_style.
*          w_alv_transf-cellstyles[] = t_style[].
*          APPEND w_alv_transf TO  t_alv_transf.
*        ENDLOOP.
*-CS2024000522-18.07.2024-JT-#147087-fim

        SORT t_alv_transf BY ped_ebeln ASCENDING.

      WHEN 'EST_REM'.

        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = tl_index_rows.

        IF lines( tl_index_rows[] ) NE 1.
          MESSAGE s024(sd) WITH 'Selecione  uma linha!' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        LOOP AT tl_index_rows INTO wl_index_rows.
          READ TABLE t_alv_transf INTO w_alv_transf INDEX wl_index_rows.
          PERFORM f_estorna_remessa USING w_alv_transf CHANGING l_estorno.
          IF l_estorno IS NOT INITIAL.
            MESSAGE 'Estorno realizado com sucesso!' TYPE 'I'.
            PERFORM f_selecao_dados_nf_propria. "*-CS2024000522-18.07.2024-JT-#143588
            PERFORM f_processa_dados.
            EXIT.
          ENDIF.
        ENDLOOP.
    ENDCASE.

*** Método de atualização de dados na Tela
    w_stable-row = 'X'.
    w_stable-col = 'X'.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

ENDCLASS.

*-CS2024000522-26.08.2024-JT-#147087-inicio
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_exibi_popup_erro
*&---------------------------------------------------------------------*
FORM f_exibir_remessas.

  CHECK t_dados_remessa[] IS NOT INITIAL.

*-----------------------------
* colunas alv
*-----------------------------
  PERFORM f_preenche_fcat USING :
   '01' ''          ''            'T_DADOS_REMESSA'  'PED_EBELN'   'Pedido'     '12'     ''    ''     ''    '' '' ' ',
   '02' ''          ''            'T_DADOS_REMESSA'  'REM_VBELN'   'Remessa'    '12'     ''    ''     ''    '' '' ' ',
   '03' ''          ''            'T_DADOS_REMESSA'  'REM_BRGEW'   'Quantidade' '15'     ''    ''     ''    '' '' ' '.

*-----------------------------
* layout
*-----------------------------
  ls_variant-report = sy-repid && 'XXX'.
  l_grid_title      = 'Remessas Geradas'.

*-----------------------------
* exibe alvv
*-----------------------------
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = sy-repid
      it_fieldcat           = it_fieldcat[]
*     it_sort               = t_sort[]
*     i_callback_user_command = 'USER_COMMAND_COMPRO'
      i_grid_title          = l_grid_title
      i_save                = 'X'
      is_variant            = ls_variant
      i_screen_start_column = 40
      i_screen_start_line   = 08
      i_screen_end_column   = 145
      i_screen_end_line     = 18
    TABLES
      t_outtab              = t_dados_remessa.

ENDFORM.

FORM f_preenche_fcat   USING  VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_hotspot)
                              VALUE(p_checkbox)
                              VALUE(p_icon).

  CLEAR: wa_estrutura.

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
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-icon          = p_icon.
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-checkbox      = p_checkbox.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_fieldcat.

ENDFORM.
*-CS2024000522-26.08.2024-JT-#147087-fim
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
