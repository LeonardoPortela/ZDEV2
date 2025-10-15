*&---------------------------------------------------------------------*
*& MODULE PAI_0100_INPUT
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  DATA: tx_centro TYPE char40.
  DATA: tx_equipamento TYPE ktx01.

  IF sy-dynnr EQ '0100'.
    DATA: r_busca_equipamento TYPE REF TO z_seleciona_dados.
    CREATE OBJECT r_busca_equipamento.

    GET CURSOR FIELD w_cursor_field.
    CASE sy-ucomm.
      WHEN 'REFRESH'.
        LEAVE TO CURRENT TRANSACTION.
      WHEN 'REL_PLAN'.
        PERFORM shdb_exibir_plan.

      WHEN 'REL_STATUS'.
        IF tbx_centro IS INITIAL.
          MESSAGE s836(sd) WITH 'Informar o centro responsavel.' DISPLAY LIKE 'E'.
          CLEAR tx_centro.
          EXIT.
        ELSE.
          PERFORM rel_vis_epto USING tbx_centro.
        ENDIF.

      WHEN 'DOUBLE'.
        PERFORM exibir_equip.

      WHEN 'BACK'
        OR 'CANC'.

        LEAVE TO SCREEN 0.

      WHEN 'BTN_BUSCAR'
        OR 'ENTER' ."OR 'RB_TRAN'.

        IF tbx_centro IS INITIAL.
          MESSAGE s836(sd) WITH 'Informar o centro responsavel.' DISPLAY LIKE 'E'.
          CLEAR tx_centro.
          EXIT.
        ELSE.
          SELECT SINGLE *
          FROM t001w
          INTO @DATA(_t001w)
          WHERE werks EQ @tbx_centro.

          IF _t001w IS NOT INITIAL.
            CLEAR tx_centro.
            tx_centro = _t001w-name1.
          ELSE.
            MESSAGE s836(sd) WITH 'Centro não existe.' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ENDIF.


*        IF TBX_EQUIPAMENTO IS INITIAL.
*          MESSAGE S836(SD) WITH 'Informar o equipamento.' DISPLAY LIKE 'E'.
*          CLEAR TX_EQUIPAMENTO.
*          EXIT.
*        ELSE.

        IF tbx_equipamento IS NOT INITIAL.
          tbx_equipamento = |{ tbx_equipamento ALPHA = IN }|.
          SELECT SINGLE *
          FROM eqkt
          INTO @DATA(_eqkt)
           WHERE equnr EQ @tbx_equipamento.

          tbx_equipamento = |{ tbx_equipamento ALPHA = OUT }|.
          IF _eqkt IS NOT INITIAL.
            tx_equipamento = _eqkt-eqktx.
          ELSE.
            MESSAGE s836(sd) WITH 'Equipamento não existe.' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ENDIF.

        IF tbx_equipamento IS NOT INITIAL.
          PERFORM check_bloq_equip USING tbx_equipamento CHANGING sy-subrc.
        ENDIF.

*        VERIFICA SE O USUÁRIO POSSUÍ PERMISSÃO PARA A PESQUISA INFORMADA.
        AUTHORITY-CHECK OBJECT 'I_IWERK' ID 'IWERK'
        FIELD tbx_centro.

        IF sy-subrc = 0.
*          IF ( TBX_CENTRO IS NOT INITIAL )."  AND TBX_EQUIPAMENTO IS NOT INITIAL ).
          r_busca_equipamento->z_seleciona_dados_tela_0110( ).
          r_busca_equipamento->z_seleciona_dados_tela_0120( ).
          r_busca_equipamento->z_seleciona_dados_tela_0130( ).

          CALL FUNCTION 'Z_DOC_CHECK_NEW'
            EXPORTING
              i_screen   = '100'
              i_show     = ''
              i_repid    = sy-repid
            IMPORTING
              e_messagem = wa_mensagem
            TABLES
              it_msgs    = it_msg_return.

        ELSE.
          EXIT.
*            MESSAGE S836(SD) WITH 'Preecher todos os campos obrigatórios.' DISPLAY LIKE 'E'.
        ENDIF.
*        ENDIF.

      WHEN 'SHOW_MSG'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen   = '100'
            i_show     = 'X'
            i_repid    = sy-repid
          IMPORTING
            e_messagem = wa_mensagem
          TABLES
            it_msgs    = it_msg_return.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
ENDMODULE.                    "PAI_0100 INPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0200 INPUT.
  CASE sy-ucomm.
      GET CURSOR FIELD w_cursor_field.
    WHEN 'BTN_CANC_OPERACAO'.
      LEAVE TO SCREEN 0.

    WHEN 'BTN_CONF_OPERACAO'.

      DATA: r_iniciar_processo_zbapis TYPE REF TO zbapis,
            r_finish_processo_zbapis  TYPE REF TO z_seleciona_dados.

      PERFORM: valida_ordens_manut.
      PERFORM: valida_info_destino.

      CHECK return_status IS INITIAL.

      CREATE OBJECT: r_iniciar_processo_zbapis,
                     r_finish_processo_zbapis.

*     Inicia processo de empréstimo do equipamento
      r_iniciar_processo_zbapis->z_iniciar_processo_emprestimo( ).

*-US 158036-26-11-2024-#158036-RJF-Início
      IF gw_erro-message IS NOT INITIAL.
        data len(3) type n.
        len = strlen( gw_erro-message ).
        IF len LT 50.
          MESSAGE i836(sd) WITH gw_erro-message
                                DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE i836(sd) WITH gw_erro-message(50)
                        gw_erro-message+50(50)
*                              gw_erro-message+100(50)
*                              gw_erro-message+150(50)
                        DISPLAY LIKE 'E'.
        ENDIF.
        EXIT.
      ENDIF.

      IF r_iniciar_processo_zbapis->gv_stopzb IS NOT INITIAL.
        EXIT.
      ENDIF.
*-US 158036-26-11-2024-#158036-RJF-Fim

*     Limpa tela após empréstimo.
      r_finish_processo_zbapis->z_atualiza_tela_emprestimo( ).

    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      IF tbx_centro_destino IS NOT INITIAL.
        SELECT SINGLE *
        FROM t001w
        INTO @DATA(w_t001w)
        WHERE werks EQ @tbx_centro_destino.
        IF w_t001w IS NOT INITIAL.
          CLEAR txt_centro_destino.
          txt_centro_destino = w_t001w-name1.
          EXIT.
        ENDIF.
        CLEAR: w_t001w, txt_centro_destino.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " PAI_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_BLOQ_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TBX_EQUIPAMENTO  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM check_bloq_equip  USING    p_tbx_equipamento
                       CHANGING p_sy_subrc.
  p_tbx_equipamento = |{ p_tbx_equipamento ALPHA = IN }|.

  CALL FUNCTION 'EQUIPMENT_READ'
    EXPORTING
*     I_HANDLE       = G_ITOB_HANDLE
      i_lock         = itob_bool-true
      equi_no        = p_tbx_equipamento
      check_auth     = yx
      x_xaktyp       = x_xaktyp
    IMPORTING
      equi           = equi
      eqkt           = eqkt
      equz           = equz
      iloa           = iloa
      eqbs           = eqbs
      fleet          = fleet
      efhm           = efhm
    EXCEPTIONS
      equi_not_found = 1
      equz_not_found = 2
      iloa_not_found = 3
      eqkt_not_found = 4
      auth_no_begrp  = 5
      auth_no_iwerk  = 6
      auth_no_swerk  = 7
      auth_no_ingrp  = 8
      auth_no_kostl  = 9
      err_handle     = 10
      lock_failure   = 11
      OTHERS         = 12.

  p_tbx_equipamento = |{ p_tbx_equipamento ALPHA = OUT }|.

  CASE p_sy_subrc.
    WHEN 0.
*         Lesen erfolgreich, keine Behandlung notwendig

    WHEN 11.
*         Sperrproblem
      PERFORM fehler_beim_sperren_f10 USING 0 space space.

    WHEN OTHERS.
*         Zugriffsfehler (z.B. Segment oder Berechtigung nicht vorh.)
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FEHLER_BEIM_SPERREN_F10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0      text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM fehler_beim_sperren_f10

  USING
    VALUE(src)   LIKE sy-subrc      "!!! value parameter
    VALUE(equnr) LIKE equi-equnr    "!!! value parameter (only SRC = 1)
    VALUE(user)  LIKE sy-msgv1.     "!!! value parameter (only SRC = 1)
  DATA:
    msg_type         LIKE sy-msgty,
    object_locked(1) TYPE c.

* in edit-mode inform calling TA if locking not successful
* (unfortunately 'x_edit_mode' is set later -> test 'sy-calld')
  IF NOT sy-calld IS INITIAL.
    object_locked = yx.
    EXPORT object_locked
      TO MEMORY ID mem_id_editequi.
    EXPORT ind_object_locked       FROM 'X'
           ind_inh_data_to_receive FROM space
           ind_update_success      FROM space
      TO MEMORY ID 'INHB'.
    msg_type = 'S'.

  ELSE.
    msg_type = 'E'.
  ENDIF.

  CASE src.
    WHEN 0.
      MESSAGE ID sy-msgid TYPE msg_type NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    WHEN 1.
      MESSAGE ID 'IS' TYPE msg_type NUMBER '813'
      WITH equnr user.

    WHEN OTHERS.
      MESSAGE ID 'IS' TYPE msg_type NUMBER '005'.
  ENDCASE.

  IF NOT sy-calld IS INITIAL.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.
