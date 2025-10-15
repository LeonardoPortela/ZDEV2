*----------------------------------------------------------------------*
*                               AMAGGI                                 *
*----------------------------------------------------------------------*
* Autor..........: RJFREITAS                                           *
* Data...........: 01.08.2022                                          *
* Descrição....  : HCM - Integração empregados SAP X APP Desenvolve    *
* Transação......: ZHCM...                                             *
* AzureDevops....: #76363 - CS2022000390                               *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor......: Ronaldo José de Freitas                                 *
* Observações: HCM - Integração empregados SAP X APP Desenvolve        *
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report  ZHCMR_PA0075
*&---------------------------------------------------------------------*
REPORT zhcmr_pa0075.

*----------------------------------------------------------------------*
* Declarações
*----------------------------------------------------------------------*
INCLUDE zhcmr_pa0075_top.

*--------------------------------------------------------------------*
*	Inicialização                                                      *
*--------------------------------------------------------------------*
INITIALIZATION.

START-OF-SELECTION.
*----------------------------------------------------------------------*
* Tela
*----------------------------------------------------------------------*
  INCLUDE zhcmr_pa0075_screen.

*----------------------------------------------------------------------*
* Processamento Principal - Início
*----------------------------------------------------------------------*
  TRY.
      go_sap_pp = NEW #( ).
      go_sap_pp->set_param( EXPORTING
                                      it_bukrs  = gr_bukrs[]
                                      it_kostl  = gr_kostl[]
                                      it_pernr  = gr_pernr[]
                                      i_endda   = p_endda
                                      i_carloc  = p_carloc
                                      i_carati  = p_carati
                                      i_caratu  = p_caratu
                                      i_cargos  = p_cargos ). " Set

      go_sap_pp->run( IMPORTING et_return  = gt_saida
                                et_returno = gt_saidaorg ). "Processamento Principal

      IF gt_saida[] IS NOT INITIAL
      AND ( p_carati IS NOT INITIAL OR
            p_caratu IS NOT INITIAL
          ).

        gt_saidalv = CORRESPONDING #( gt_saida ).

        IF sy-batch IS INITIAL.

          go_report = NEW #( ).
          IF go_report IS NOT BOUND.
            LEAVE SCREEN.
          ENDIF.

          IF go_alv IS INITIAL.
* Objeto para Estrutura do ALV
            go_report->set_field_cat(
              EXPORTING iv_container_name = space
              CHANGING  co_alv            = go_alv
                        co_report         = go_report
                        ct_table          = gt_saidalv ).

* Retorna Colunas da Fieldcat
            go_t_columns = go_alv->get_columns( ).

* Ajustar colunas de acordo com conteúdo
            go_t_columns->set_optimize( abap_true ).

* PF status local
            go_report->set_screen_status( EXPORTING p_pfstatus = 'SALV'
                                          CHANGING co_alv = go_alv ).

* Coluna de seleção
            go_report->set_selection_mode(
               EXPORTING p_value = if_salv_c_selection_mode=>row_column
               CHANGING  co_alv   = go_alv ).

* Altera descrição das colunas
            go_report->set_text( EXPORTING p_column = 'ICON'
                                           p_long_text = text-016
                                           p_medium_text = text-016
                                           p_short_text = text-016 ).

* Altera descrição das colunas
            go_report->set_text( EXPORTING p_column = 'CODE'
                                           p_long_text = text-001
                                           p_medium_text = text-001
                                           p_short_text = text-001 ).

            go_report->set_text( EXPORTING p_column = 'NAME'
                                           p_long_text = text-002
                                           p_medium_text = text-002
                                           p_short_text = text-002 ).

            go_report->set_text( EXPORTING p_column = 'LOGIN'
                                           p_long_text = text-003
                                           p_medium_text = text-003
                                           p_short_text = text-003 ).

            go_report->set_text( EXPORTING p_column = 'PASSWORD'
                                           p_long_text = text-004
                                           p_medium_text = text-004
                                           p_short_text = text-004 ).

            go_report->set_text( EXPORTING p_column = 'STATUS'
                                           p_long_text = text-005
                                           p_medium_text = text-005
                                           p_short_text = text-005 ).

            go_report->set_text( EXPORTING p_column = 'EMAIL'
                                           p_long_text = text-006
                                           p_medium_text = text-006
                                           p_short_text = text-006 ).

            go_report->set_text( EXPORTING p_column = 'PHONE'
                                           p_long_text = text-007
                                           p_medium_text = text-007
                                           p_short_text = text-007 ).

            go_report->set_text( EXPORTING p_column = 'POSITIONCODE'
                                           p_long_text = text-008
                                           p_medium_text = text-008
                                           p_short_text = text-008 ).

            go_report->set_text( EXPORTING p_column = 'LOCATIONCODE'
                                           p_long_text = text-009
                                           p_medium_text = text-009
                                           p_short_text = text-009 ).

            go_report->set_text( EXPORTING p_column = 'STATIONCODE'
                                           p_long_text = text-010
                                           p_medium_text = text-010
                                           p_short_text = text-010 ).

            go_report->set_text( EXPORTING p_column = 'STATIONNAME'
                                           p_long_text = text-011
                                           p_medium_text = text-011
                                           p_short_text = text-011 ).

            go_report->set_text( EXPORTING p_column = 'TEAMCODE'
                                           p_long_text = text-012
                                           p_medium_text = text-012
                                           p_short_text = text-012 ).

            go_report->set_text( EXPORTING p_column = 'TEAMNAME'
                                           p_long_text = text-013
                                           p_medium_text = text-013
                                           p_short_text = text-013 ).

            go_report->set_text( EXPORTING p_column = 'TEAMLEADERCODE'
                                           p_long_text = text-014
                                           p_medium_text = text-014
                                           p_short_text = text-014 ).

            go_report->set_text( EXPORTING p_column = 'PROFILECODE'
                                           p_long_text = text-015
                                           p_medium_text = text-015
                                           p_short_text = text-015 ).

            DATA: go_click     TYPE REF TO cl_click.
            go_click = NEW #( ).
            go_events = go_alv->get_event( ).
            SET HANDLER: go_click->on_user_command FOR go_events.

* Exibe ALV
            go_alv->display( ).
          ELSE.
            CHECK go_alv IS BOUND.
            go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).

          ENDIF.
        ELSE.
* Processo back
          IF gt_saida[] IS NOT INITIAL.
            CLEAR gv_error.
*            DATA: lv_idx TYPE i.
*            LOOP AT gt_saida INTO gw_saida.
*              lv_idx = lv_idx + 1.
*              APPEND gw_saida TO gt_saidax.
*              CLEAR gw_saida.
*              go_sap_pp->set_api( EXPORTING it_saida = gt_saidax

            go_sap_pp->set_api( EXPORTING it_saida = gt_saida
                                IMPORTING ev_stat  = DATA(lv_stat)
                                          ev_error = DATA(lv_error)
                                          ev_code  = DATA(lv_code)
                                          ev_text  = DATA(lv_text) ).

*            ENDLOOP.
          ENDIF.
        ENDIF.
      ELSE.
        IF sy-batch IS INITIAL
       AND p_carloc IS NOT INITIAL
       OR  p_carati IS NOT INITIAL
       OR  p_caratu IS NOT INITIAL.
          IF gt_saidaorg[] IS INITIAL.
            MESSAGE 'Nenhum resultado encontrado!'(i02) TYPE 'I' DISPLAY LIKE 'I'.
          ELSE.
            MESSAGE 'Processamento realizado!'(i05) TYPE 'I' DISPLAY LIKE 'I'.
          ENDIF.
        ENDIF.
      ENDIF.

*    CATCH  cx_sy_arithmetic_error INTO lo_oref.
    CATCH zcx_error INTO DATA(ex_erro).
      IF sy-batch IS INITIAL.
        MESSAGE ex_erro->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.
  ENDTRY.
