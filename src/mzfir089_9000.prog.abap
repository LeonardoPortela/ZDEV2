*----------------------------------------------------------------------*
***INCLUDE MZDRE0001_9000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  DATA: xnivel TYPE znivel_dre.

  IF tl_9000 IS INITIAL.
    tl_9000 = tl_9001.
    PERFORM atualiza_alv_est.
    xnivel = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
    qt_char_nivel = strlen( xnivel ).
  ENDIF.

  IF tl_9000 EQ tl_9001.
    SET PF-STATUS 'PFEST'.
    SET TITLEBAR 'TLEST'.
  ELSEIF tl_9000 = tl_9003.
    SET PF-STATUS 'PF9003E'.
*    SET TITLEBAR 'TL9003' WITH wa_zfit181_alv-cod_estrutura.

    SET TITLEBAR 'TL9003'.


  ENDIF.

ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000_exit INPUT.

  IF tl_9000 = tl_9001.
    LEAVE PROGRAM.
  ELSEIF tl_9000 = tl_9003.
    tl_9000 = tl_9001.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_9000_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE ok_code_9000.
    WHEN ok_atualiza.
      PERFORM atualiza_alv_est.
    WHEN ok_nova_est.
      PERFORM cria_est.
    WHEN ok_copi_est.
      PERFORM copiar_est.
    WHEN ok_dele_est.
      PERFORM apagar_est.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_DRE_EST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_est OUTPUT.

  CONSTANTS: tabela_dre_est TYPE string VALUE 'IT_ZFIT181_ALV'.

  DATA: text_e001 TYPE c LENGTH 50 VALUE 'Empresa',
        text_e002 TYPE c LENGTH 50 VALUE 'Nome Empresa',
        text_e003 TYPE c LENGTH 50 VALUE 'Estrutura',
        text_e004 TYPE c LENGTH 50 VALUE 'Nome Estrutura'.

  IF prim_dre_est IS INITIAL.

    CREATE OBJECT dre_container_est
      EXPORTING
        container_name = 'ALV'.

    CREATE OBJECT dre_alv_est
      EXPORTING
        i_parent = dre_container_est.

    PERFORM z_estrutura_fieldcat TABLES dre_catalogo_est USING:
        tabela_dre_est 'EDITAR' ' '       'X' 01 04 space space space 'X'   space space space,
        tabela_dre_est 'ESTRUT' ' '       'X' 02 04 space space space 'X'   space space space,
        tabela_dre_est 'BUKRS'  text_e001 ' ' 03 05 space space space space space space space,
        tabela_dre_est 'BUTXT'  text_e002 ' ' 04 40 space space space space space space space,
        tabela_dre_est 'COD_ESTRUTURA'  text_e003 ' ' 05 05 space space space space space space space,
        tabela_dre_est 'NOME_ESTRUTURA'  text_e004 ' ' 06 60 space space space space space space space.

    CLEAR: dre_gs_layout.
    dre_gs_layout-zebra         = c_x.
*    DRE_GS_LAYOUT-CWIDTH_OPT    = C_X.
    dre_gs_layout-sel_mode      = 'A'..

    CALL METHOD dre_alv_est->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = dre_gs_layout
      CHANGING
        it_fieldcatalog = dre_catalogo_est
        it_outtab       = it_zfit181_alv[].

*   Create Object for Event Handler
    CREATE OBJECT dre_event_dre_est.
    SET HANDLER dre_event_dre_est->handle_hotspot_dre_est FOR dre_alv_est.

    prim_dre_est = c_x.
  ENDIF.

  CALL METHOD dre_alv_est->refresh_table_display.

  CALL METHOD dre_alv_est->set_scroll_info_via_id
    EXPORTING
      is_col_info = wa_scroll_col
      is_row_no   = wa_scroll_row.

ENDMODULE.                 " CRIA_ALV_DRE_EST  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_ALV_EST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_alv_est .

  DATA: it_t001 TYPE TABLE OF t001 WITH HEADER LINE.

  CLEAR: it_zfit181[],
         it_zfit181_alv[].

  SELECT * INTO TABLE it_zfit181
    FROM zfit181.

  SELECT * INTO TABLE it_t001
    FROM t001.

  LOOP AT it_zfit181.
    CLEAR: it_zfit181_alv.
    MOVE-CORRESPONDING it_zfit181 TO it_zfit181_alv.
    READ TABLE it_t001 WITH KEY bukrs = it_zfit181-bukrs.
    it_zfit181_alv-butxt  = it_t001-butxt.
    it_zfit181_alv-editar = icon_change.
    it_zfit181_alv-estrut = icon_display_tree.
    APPEND it_zfit181_alv.
  ENDLOOP.

ENDFORM.                    " ATUALIZA_ALV_EST

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.

  SET PF-STATUS 'PF9003'.
  SET TITLEBAR 'TL9002'.

  IF vg_editar IS NOT INITIAL.
    LOOP AT SCREEN.
      IF ( screen-name EQ 'ZFIT181-BUKRS' ) OR ( screen-name EQ 'ZFIT181-COD_ESTRUTURA' ).
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CLEAR: vg_bukrs_txt.

  IF NOT zfit181-bukrs IS INITIAL.
    SELECT SINGLE butxt INTO vg_bukrs_txt
      FROM t001
     WHERE bukrs EQ zfit181-bukrs.
  ENDIF.

ENDMODULE.                 " STATUS_9002  OUTPUT


FORM cria_est .

  CLEAR: vg_editar, zfit181.

  CALL SCREEN 9002 STARTING AT 10 10.

ENDFORM.                    " CRIA_EST

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_9002_EXIT  INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.

  IF ok_code_9002 EQ c_conf.

    SELECT SINGLE * INTO wa_zfit181
      FROM zfit181
     WHERE bukrs EQ zfit181-bukrs
       AND cod_estrutura EQ zfit181-cod_estrutura.

    IF ( sy-subrc IS INITIAL ) AND ( vg_editar IS INITIAL ).
      MESSAGE 'Empresa e Estrutura já cadastrada!' TYPE 'S'.
    ELSE.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF wa_zfit181
      FROM zgl001_dre_est
     WHERE bukrs EQ zfit181-bukrs
       AND versn EQ zfit181-cod_estrutura.

      IF ( sy-subrc IS INITIAL ).
        MESSAGE 'Empresa e Estrutura já cadastrada em modelo antigo!' TYPE 'S'.
      ELSE.
        MODIFY zfit181.
        PERFORM atualiza_alv_est.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_9002  INPUT

*&---------------------------------------------------------------------*
*&      Module  Z_NOME_EMPRESA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_check_code_emp_9002 INPUT.

  PERFORM f_check_cod_emp USING zfit181-bukrs.

ENDMODULE.                 " Z_NOME_EMPRESA  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9003 INPUT.

  TYPES: BEGIN OF ty_nivel.
  TYPES: sign       TYPE char01,
* ---> S4 Migration - 19/06/2023 - JS
*         option TYPE char02,
         option(02) TYPE c,
* <--- S4 Migration - 19/06/2023 - JS
         low        TYPE znivel_dre,
         high       TYPE znivel_dre.
  TYPES: END OF ty_nivel.


  DATA: node      TYPE tv_nodekey,
        nodet     TYPE tv_nodekey,
        node1     TYPE tv_nodekey,
        node_emp  TYPE zfit184-empresa,
        vg_nivel  TYPE znivel_dre,
        vg_nivelc TYPE znivel_dre,
        wa_est02  TYPE zfit182,
        nx        TYPE i,
        nv00      TYPE char05,
        ftfiltro  TYPE TABLE OF ty_nivel WITH HEADER LINE,
        answer    TYPE c LENGTH 1.

  CLEAR: node, vg_nivel, wa_fluxo_est02.

  CASE ok_code_9000.

    WHEN ok_cod_fluxo.

      CALL METHOD g_tree->get_selected_node
        IMPORTING
          node_key                   = node
        EXCEPTIONS
          failed                     = 1
          single_node_selection_only = 2
          cntl_system_error          = 3
          OTHERS                     = 4.

      IF ( sy-subrc IS INITIAL ) AND ( NOT node IS INITIAL ).

        FIND 'VC' IN node.
        IF sy-subrc <> 0.
          MESSAGE 'Cód. de fluxo de caixa só pode criado como filho do "VC" ' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

*        IF node(2) = 'FD'.
        IF node(2) = 'FD' OR node(2) = 'EN' OR node(2) = 'SA' OR node(2) = 'ZE'. " RJF
          DATA(lt_tam) = strlen( node ).
          IF lt_tam <= 5. "Node é FD e não tem filho.
            MESSAGE 'Cód. de fluxo de caixa só pode criado como filho do "VC" ' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ELSEIF ( node(2) EQ 'SI' ) OR ( node(2) EQ 'SC' ) OR ( node(2) EQ 'S+' ) OR ( node(2) EQ 'SF' ).
          MESSAGE 'Cód. de fluxo de caixa só pode criado como filho do "VC" ' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

**  Begin of CS2022000708  #83806 FF   15.12.2022 10:22:14
*        IF node(2) = 'FD'.
        IF node(2) = 'FD' OR node(4) = 'ENFD' OR node(4) = 'SAFD' OR node(4) = 'ZEFD'. " RJF
          DATA lv_cont TYPE i.
          CLEAR lv_cont.
          WHILE node IS NOT INITIAL.
            lv_cont = lv_cont + 1.
            IF NOT vg_nivel IS INITIAL.
              CONCATENATE vg_nivel '.' INTO vg_nivel.
            ENDIF.
*            IF  lv_cont = 1.
            IF ( lv_cont = 1 OR ( lv_cont < 3 AND ( node(4) = 'ENFD' OR nodet(4) = 'ENFD'
                                                 OR node(4) = 'SAFD' OR nodet(4) = 'SAFD'
                                                 OR node(4) = 'ZEFD' OR nodet(4) = 'ZEFD') ) ). " RJF
              nv00 = node(5).

*-----------

              IF node(4) EQ 'ENFD' OR node(4) EQ 'SAFD' OR node(4) EQ 'ZEFD'. " RJF
                nodet = node. " RJF
                nv00 = node+2(5).   " RJF...

              ENDIF.

              IF node(4) EQ 'ENFD' OR nodet(4) EQ 'ENFD' OR node(4) EQ 'SAFD' OR nodet(4) EQ 'SAFD' OR node(4) EQ 'ZEFD' OR nodet(4) EQ 'ZEFD'. " RJF
                nodet = node. " RJF
                IF lv_cont EQ 1.
                  nv00 = node(2).   " RJF
                  CONCATENATE  vg_nivel node(2) INTO vg_nivel.
                ELSE.
                  CONCATENATE  vg_nivel node(5) INTO vg_nivel.
                ENDIF.

*              CONCATENATE  vg_nivel node(2) '.' node(5) INTO vg_nivel.

                IF lv_cont EQ 1.
                  nx = strlen( node ).
                  nx = nx - 2.
                  IF nx <= 0.
                    CLEAR node.
                  ELSE.
                    node = node+2(nx).
                  ENDIF.
                ELSE.
                  nx = strlen( node ).
                  nx = nx - 5.
                  IF nx <= 0.
                    CLEAR node.
                  ELSE.
                    node = node+5(nx).
                  ENDIF.
                ENDIF.

              ELSE.  " RJF - fim

*-----------

                CONCATENATE vg_nivel node(5) INTO vg_nivel.

                nx = strlen( node ).
                nx = nx - 5.
                IF nx <= 0.
                  CLEAR node.
                ELSE.
                  node = node+5(nx).
                ENDIF.

              ENDIF. " RJF

            ELSE.
              nv00 = node(2).
              CONCATENATE vg_nivel node(2) INTO vg_nivel.
              nx = strlen( node ).
              nx = nx - 2.
              IF nx <= 0.
                CLEAR node.
              ELSE.
                node = node+2(nx).
              ENDIF.
            ENDIF.
          ENDWHILE.
        ELSE.

          WHILE node IS NOT INITIAL.
            IF NOT vg_nivel IS INITIAL.
              CONCATENATE vg_nivel '.' INTO vg_nivel.
            ENDIF.
            CONCATENATE vg_nivel node(2) INTO vg_nivel.
            nx = strlen( node ).
            nx = nx - 2.
            IF nx EQ 0.
              CLEAR node.
            ELSE.
              node = node+2(nx).
            ENDIF.
          ENDWHILE.
        ENDIF.

        SELECT SINGLE * INTO zfit182 "RJF
         FROM zfit182
        WHERE bukrs EQ wa_zfit181-bukrs
          AND cod_estrutura EQ wa_zfit181-cod_estrutura
          AND nivel EQ vg_nivel.

        CLEAR: zfit183.
        CALL SCREEN 9006 STARTING AT 10 10.

      ELSE.
        MESSAGE 'Selecione um Nível para Inserção do Cód. de Fluxo de Caixa!' TYPE 'S'.
      ENDIF.

    WHEN c_excluir.

      CALL METHOD g_tree->get_selected_node
        IMPORTING
          node_key                   = node
        EXCEPTIONS
          failed                     = 1
          single_node_selection_only = 2
          cntl_system_error          = 3
          OTHERS                     = 4.

      IF ( sy-subrc IS INITIAL ) AND ( NOT node IS INITIAL ).

        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            titel     = 'Atenção!'
            textline1 = 'Todo o nível da estrutura será excluido!'
            textline2 = 'Deseja realmente excluir?'
          IMPORTING
            answer    = answer.

        IF answer = c_j.

          IF node(2) = 'CR' OR
             node(2) = 'CC'.
            IF node+2(1) = '_'.
              node1 = node.
              SPLIT node1 AT '_' INTO node node_emp.
            ENDIF.
          ENDIF.

*          IF node(2) = 'FD'.
          IF node(2) = 'FD' OR node(4) = 'ENFD' OR node(4) = 'SAFD' OR node(4) = 'ZEFD'. " RJF

            CLEAR lv_cont.
            WHILE node IS NOT INITIAL.
              lv_cont = lv_cont + 1.
              IF NOT vg_nivel IS INITIAL.
                CONCATENATE vg_nivel '.' INTO vg_nivel.
              ENDIF.
              IF lv_cont = 1 OR ( nodet(2) NE 'FD' AND ( lv_cont EQ 2 ) ).
                nv00 = node(5).
*
                IF node(4) EQ 'ENFD' OR nodet(4) EQ 'ENFD' OR node(4) EQ 'SAFD' OR nodet(4) EQ 'SAFD' OR node(4) EQ 'ZEFD' OR nodet(4) EQ 'ZEFD'. " RJF ini


                  nodet = node. " RJF
                  IF lv_cont = 1.
                    nv00 = node(2).   " RJF
                    CONCATENATE  vg_nivel node(2) INTO vg_nivel.
                  ELSE.
                    CONCATENATE  vg_nivel node(5) INTO vg_nivel.
                  ENDIF.

                  IF lv_cont EQ 1.
                    nx = strlen( node ).
                    nx = nx - 2.
                    IF nx <= 0.
                      CLEAR node.
                    ELSE.
                      node = node+2(nx).
                    ENDIF.
                  ELSE.
                    nx = strlen( node ).
                    nx = nx - 5.
                    IF nx <= 0.
                      CLEAR node.
                    ELSE.
                      node = node+5(nx).
                    ENDIF.
                  ENDIF.

                ELSE.  " RJF - fim

                  CONCATENATE vg_nivel node(5) INTO vg_nivel.
                  nx = strlen( node ).
                  nx = nx - 5.
                  IF nx <= 0.
                    CLEAR node.
                  ELSE.
                    node = node+5(nx).
                  ENDIF.

                ENDIF.
              ELSE.
                nv00 = node(2).
                CONCATENATE vg_nivel node(2) INTO vg_nivel.
                nx = strlen( node ).
                nx = nx - 2.
                IF nx <= 0.
                  CLEAR node.
                ELSE.
                  node = node+2(nx).
                ENDIF.
              ENDIF.
            ENDWHILE.
          ELSEIF node(2) = 'CC'.

            DELETE FROM zfit184
             WHERE bukrs EQ wa_zfit181-bukrs
               AND cod_estrutura EQ wa_zfit181-cod_estrutura
               AND empresa EQ node_emp.
          ELSEIF node(2) = 'CR'.

            DELETE FROM zfit183
             WHERE bukrs EQ wa_zfit181-bukrs
               AND cod_estrutura EQ wa_zfit181-cod_estrutura
               AND codigo_fluxo EQ node_emp.
          ELSE.

            WHILE node IS NOT INITIAL.
              IF NOT vg_nivel IS INITIAL.
                CONCATENATE vg_nivel '.' INTO vg_nivel.
              ENDIF.
              CONCATENATE vg_nivel node(2) INTO vg_nivel.
              nx = strlen( node ).
              nx = nx - 2.
              IF nx EQ 0.
                CLEAR node.
              ELSE.
                node = node+2(nx).
              ENDIF.
            ENDWHILE.
          ENDIF.

          IF vg_nivel IS NOT INITIAL.

            IF vg_nivel(2) EQ 'EN' OR vg_nivel(2) EQ 'SA' OR vg_nivel(2) EQ 'ZE'.







              vg_nivelc = vg_nivel && '%'.

              DELETE FROM zfit183
               WHERE bukrs EQ wa_zfit181-bukrs
                 AND cod_estrutura EQ wa_zfit181-cod_estrutura
                 AND nivel LIKE vg_nivelc.

              DELETE FROM zfit182
               WHERE bukrs EQ wa_zfit181-bukrs
                 AND cod_estrutura EQ wa_zfit181-cod_estrutura
                 AND nivel LIKE vg_nivelc.

            ELSE.
              DELETE FROM zfit183
               WHERE bukrs EQ wa_zfit181-bukrs
                 AND cod_estrutura EQ wa_zfit181-cod_estrutura
                 AND nivel EQ vg_nivel.

              DELETE FROM zfit182
               WHERE bukrs EQ wa_zfit181-bukrs
                 AND cod_estrutura EQ wa_zfit181-cod_estrutura
                 AND nivel EQ vg_nivel.
            ENDIF.
*            ftfiltro-sign   = 'I'.
*            IF qt_char_nivel > strlen( vg_nivel ).
*              CONCATENATE vg_nivel '*' INTO vg_nivel.
*              ftfiltro-option = 'CP'.
*            ELSE.
*              ftfiltro-option = 'EQ'.
*            ENDIF.
*            ftfiltro-low    = vg_nivel.
*            APPEND ftfiltro.
*
*            DELETE FROM zfit184
*            WHERE bukrs EQ wa_zfit181-bukrs
*              AND cod_estrutura EQ wa_zfit181-cod_estrutura
*              AND nivel IN ftfiltro.
*
*            DELETE FROM zfit183
*             WHERE bukrs EQ wa_zfit181-bukrs
*               AND cod_estrutura EQ wa_zfit181-cod_estrutura
*               AND nivel IN ftfiltro.
*
*            DELETE FROM zfit182
*             WHERE bukrs EQ wa_zfit181-bukrs
*               AND cod_estrutura EQ wa_zfit181-cod_estrutura
*               AND nivel IN ftfiltro.

          ENDIF.

          CALL METHOD g_tree->delete_all_nodes.
          prim_dre_nivel_re = c_x.
          PERFORM atualiza_alv_est.
          MESSAGE 'Nível Excluido!' TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE 'Selecione a linha completa para excluí-la' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN c_insert OR c_editar.

      CALL METHOD g_tree->get_selected_node
        IMPORTING
          node_key                   = node
        EXCEPTIONS
          failed                     = 1
          single_node_selection_only = 2
          cntl_system_error          = 3
          OTHERS                     = 4.

      CLEAR: vg_nivel, wa_zfit182, vg_editar.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSEIF node IS INITIAL.

        IF ok_code_9000 EQ c_insert.

          qt_niveis = 0.
          wa_zfit182-bukrs         = wa_zfit181-bukrs.
          wa_zfit182-cod_estrutura = wa_zfit181-cod_estrutura.
          MOVE-CORRESPONDING wa_zfit182 TO zfit182.
          CALL SCREEN 9004 STARTING AT 10 10.
        ELSE.
          MESSAGE 'Selecione um Nível para Inserção de SubNível!' TYPE 'S'.
        ENDIF.
      ELSE.

        IF ok_code_9000 = c_editar.

          FIND 'VC' IN node.
          IF sy-subrc = 0.
            MESSAGE  'Edição indisponível para esta linha' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          IF node(2) = 'CR' OR
             node(2) = 'CC' OR
             node(2) = 'SI' OR
             node(2) = 'SC' OR
             node(2) = 'S+' OR
             node(2) = 'SF'.
            MESSAGE  'Edição indisponível para esta linha' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ENDIF.

        IF node(2) = 'CR' OR
           node(2) = 'CC'.
          MESSAGE  'Função INSERIR, indisponível para este nível' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        DATA(lt_node_tam) = strlen( node ).
        IF lt_node_tam > 3. "Node FD
          lt_node_tam = lt_node_tam - 2. "Pegando a última posição
          DATA(ultimo_node) = node+lt_node_tam(2).
        ENDIF.

* RJF - ini
        IF node(4) EQ 'ENFD' OR node(4) EQ 'SAFD' OR node(4) EQ 'ZEFD'.
          IF lt_node_tam > 3. "Node EN
            lt_node_tam = lt_node_tam - 5. "Pegando a última posição
            ultimo_node = node+lt_node_tam(2).
          ENDIF.
        ENDIF.
        " RJF - Fim

        IF ultimo_node(2) EQ 'VC' AND ok_code_9000 = c_insert.
          MESSAGE 'No nível VC só é possível inserir Cód. de Fluxo de Caixa.' TYPE 'S' DISPLAY LIKE 'E'.
          CLEAR ultimo_node.
          EXIT.
*        ELSEIF ( node(2) EQ 'SI' OR node(2) EQ 'SC' OR node(2) EQ 'SF' ) AND ok_code_9000 = c_insert. " RJF
        ELSEIF ( node(2) EQ 'SI' OR node(2) EQ 'SC' OR node(2) EQ 'SF' OR node(2) EQ 'ST' ) AND ok_code_9000 = c_insert. " RJF
          CONCATENATE 'Não é possível criar SubNível para o nível' node(2) INTO DATA(lv_mensagem) SEPARATED BY space.
          MESSAGE  lv_mensagem TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        qt_niveis = 0.

**  Begin of CS2022000708  #83806 FF   15.12.2022 10:22:14
        IF node(2) = 'FD' OR node(4) = 'ENFD' OR node(4) = 'SAFD' OR node(4) = 'ZEFD'.
          WHILE node IS NOT INITIAL.
            IF NOT vg_nivel IS INITIAL.
              CONCATENATE vg_nivel '.' INTO vg_nivel.
            ENDIF.
            nv00 = node(5).

            IF node(4) EQ 'ENFD' OR nodet(4) EQ 'ENFD' OR node(4) EQ 'SAFD' OR nodet(4) EQ 'SAFD' OR node(4) EQ 'ZEFD' OR nodet(4) EQ 'ZEFD'. " RJF
              nodet = node. " RJF
              IF qt_niveis IS INITIAL.
                nv00 = node(2).   " RJF
                CONCATENATE  vg_nivel node(2) INTO vg_nivel.
              ELSE.
                CONCATENATE  vg_nivel node(5) INTO vg_nivel.
              ENDIF.

*              CONCATENATE  vg_nivel node(2) '.' node(5) INTO vg_nivel.

              IF qt_niveis IS INITIAL.
                nx = strlen( node ).
                nx = nx - 2.
                IF nx <= 0.
                  CLEAR node.
                ELSE.
                  node = node+2(nx).
                ENDIF.
              ELSE.
                nx = strlen( node ).
                nx = nx - 5.
                IF nx <= 0.
                  CLEAR node.
                ELSE.
                  node = node+5(nx).
                ENDIF.
              ENDIF.

            ELSE.  " RJF - fim

              CONCATENATE vg_nivel node(5) INTO vg_nivel.

              nx = strlen( node ).
              nx = nx - 5.
              IF nx <= 0.
                CLEAR node.
              ELSE.
                node = node+5(nx).
              ENDIF.

            ENDIF. " RJF

            qt_niveis = qt_niveis + 1.

            CASE qt_niveis.
              WHEN 1.
                wa_fluxo_est02-na01 = nv00.
                wa_fluxo_est02-nv01 = nv00.
              WHEN 2.
                wa_fluxo_est02-na02 = nv00.
                wa_fluxo_est02-nv02 = nv00.
              WHEN 3.
                wa_fluxo_est02-na03 = nv00.
                wa_fluxo_est02-nv03 = nv00.
              WHEN 4.
                wa_fluxo_est02-na04 = nv00.
                wa_fluxo_est02-nv04 = nv00.
              WHEN 5.
                wa_fluxo_est02-nv05 = nv00.
            ENDCASE.
          ENDWHILE.

          SELECT SINGLE * INTO wa_est02
           FROM zfit182
          WHERE bukrs EQ wa_zfit181-bukrs
            AND cod_estrutura EQ wa_zfit181-cod_estrutura
*             AND item_estrutura = vg_nivel+3(1)
            AND nivel EQ vg_nivel.

        ELSE.
** End of FF  15.12.2022 10:22:14
          WHILE node IS NOT INITIAL.
            IF NOT vg_nivel IS INITIAL.
              CONCATENATE vg_nivel '.' INTO vg_nivel.
            ENDIF.
            nv00 = node(2).
            CONCATENATE vg_nivel node(2) INTO vg_nivel.
            nx = strlen( node ).
            nx = nx - 2.
            IF nx EQ 0.
              CLEAR node.
            ELSE.
              node = node+2(nx).
            ENDIF.
            qt_niveis = qt_niveis + 1.

            CASE qt_niveis.
              WHEN 1.
                wa_fluxo_est02-na01 = nv00.
                wa_fluxo_est02-nv01 = nv00.
              WHEN 2.
                wa_fluxo_est02-na02 = nv00.
                wa_fluxo_est02-nv02 = nv00.
              WHEN 3.
                wa_fluxo_est02-na03 = nv00.
                wa_fluxo_est02-nv03 = nv00.
              WHEN 4.
                wa_fluxo_est02-na04 = nv00.
                wa_fluxo_est02-nv04 = nv00.
              WHEN 5.
                wa_fluxo_est02-nv05 = nv00.
            ENDCASE.
          ENDWHILE.

          SELECT SINGLE * INTO wa_est02
           FROM zfit182
          WHERE bukrs EQ wa_zfit181-bukrs
            AND cod_estrutura EQ wa_zfit181-cod_estrutura
            AND nivel EQ vg_nivel.

        ENDIF.

        IF sy-subrc IS INITIAL.
          IF ok_code_9000 EQ c_insert.
**  Begin of CS2022000708  #83806 FF   13.12.2022 15:06:47
            IF wa_est02-nivel IS NOT INITIAL.
              DATA(lv_size) = strlen( wa_est02-nivel ) - 2.
              DATA(lv_nivel) = wa_est02-nivel+lv_size(2).
              IF lv_nivel = 'VC'.
                MESSAGE 'Não é possível criar SubNível para o nível VC.' TYPE 'E'.
              ENDIF.
            ENDIF.
** End of FF  13.12.2022 15:06:47
            wa_fluxo_est02-bukrs             = wa_zfit181-bukrs.
            wa_fluxo_est02-cod_estrutura     = wa_zfit181-cod_estrutura.
            wa_fluxo_est02-nitxt_ant         = wa_est02-nitxt.
            wa_fluxo_est02-nivel_ant         = wa_est02-nivel.
            CLEAR: wa_est02-nivel.

            IF qt_char_nivel EQ strlen( vg_nivel ).
              MESSAGE 'Não permitido mais níveis!' TYPE 'E'.
            ENDIF.

          ELSEIF ok_code_9000 EQ c_editar.
            wa_fluxo_est02-bukrs          = wa_zfit181-bukrs.
            wa_fluxo_est02-cod_estrutura  = wa_zfit181-cod_estrutura.
            wa_fluxo_est02-item_estrutura = wa_est02-item_estrutura.
            wa_fluxo_est02-nitxt          = wa_est02-nitxt.
            wa_fluxo_est02-nivel_ant      = wa_est02-nivel_ant.

            qt_niveis = qt_niveis - 1.

            IF NOT wa_est02-nivel_ant IS INITIAL.

              SELECT SINGLE * INTO wa_est02
              FROM zfit182
             WHERE bukrs EQ wa_zfit181-bukrs
               AND cod_estrutura EQ wa_zfit181-cod_estrutura
               AND nivel EQ wa_est02-nivel_ant.

              wa_fluxo_est02-nitxt_ant = wa_est02-nitxt.
            ENDIF.

            vg_editar = c_x.
          ENDIF.

          MOVE-CORRESPONDING wa_fluxo_est02 TO zfit182.
**  Begin of CS2022000708  #83806 FF   13.12.2022 20:57:30
          IF vg_nivel(2) = 'S+'.

            SELECT SINGLE * INTO zfit182
          FROM zfit182
           WHERE bukrs EQ wa_zfit181-bukrs
             AND cod_estrutura EQ wa_zfit181-cod_estrutura
             AND nivel EQ vg_nivel.

            CALL SCREEN 9007 STARTING AT 10 10. "Cadastro de Empresa - Saldos aplicação
          ELSE.
** End of FF  13.12.2022 20:57:30
            CALL SCREEN 9004 STARTING AT 10 10.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR ok_code_9000.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9003  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9003 OUTPUT.
  PERFORM cria_alv_est_nivel.
ENDMODULE.                 " STATUS_9003  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_DRE_EST_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cria_alv_est_nivel .

  DATA: node_table TYPE treev_ntab,
        item_table TYPE item_table_type,
        node_esp   TYPE tv_nodekey.

  IF prim_dre_nivel IS INITIAL.
    CREATE OBJECT g_application.
    PERFORM create_and_init_tree.
    prim_dre_nivel    = c_x.
    prim_dre_nivel_re = c_x.
  ENDIF.

  IF NOT prim_dre_nivel_re IS INITIAL.

    PERFORM build_node_and_item_table USING node_table item_table.

    CALL METHOD g_tree->add_nodes_and_items
      EXPORTING
        node_table                     = node_table
        item_table                     = item_table
        item_table_structure_name      = 'MTREEITM'
      EXCEPTIONS
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    IF NOT zfit182 IS INITIAL.

      REPLACE ALL OCCURRENCES OF REGEX '[.]' IN zfit182-nivel WITH ''.
      node_esp = zfit182-nivel.

      IF zfit182-nivel = 'FD'.
        CONCATENATE zfit182-nivel '_' zfit182-item_estrutura INTO node_esp.
      ENDIF.

* RJF - ini
      IF zfit182-nivel = 'EN.FD' OR zfit182-nivel = 'SA.FD' OR zfit182-nivel = 'ZE.FD'.
        CONCATENATE zfit182-nivel '_' zfit182-item_estrutura INTO node_esp.
      ENDIF.
* RJF - Fim

      CALL METHOD g_tree->expand_node
        EXPORTING
          node_key = node_esp.

    ENDIF.

    CLEAR: prim_dre_nivel_re, zfit182.
  ENDIF.

ENDFORM.                    " CRIA_ALV_DRE_EST_NIVEL

*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_and_init_tree .

  DATA: events TYPE cntl_simple_events,
        event  TYPE cntl_simple_event.

* create a container for the tree control
  CREATE OBJECT g_custom_container
    EXPORTING
      container_name              = 'TREE_CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

* create a list tree
  CREATE OBJECT g_tree
    EXPORTING
      parent                      = g_custom_container
      node_selection_mode         = cl_gui_list_tree=>node_sel_mode_single
      item_selection              = 'X'
      with_headers                = ' '
    EXCEPTIONS
      cntl_system_error           = 1
      create_error                = 2
      failed                      = 3
      illegal_node_selection_mode = 4
      lifetime_error              = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE a000(tree_control_msg).
  ENDIF.

* define the events which will be passed to the backend
  " node double click
  event-eventid = cl_gui_list_tree=>eventid_node_double_click.
  event-appl_event = 'X'.                                   "
  APPEND event TO events.

  " item double click
  event-eventid = cl_gui_list_tree=>eventid_item_double_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " expand no children
  event-eventid = cl_gui_list_tree=>eventid_expand_no_children.
  event-appl_event = 'X'.
  APPEND event TO events.

  " link click
  event-eventid = cl_gui_list_tree=>eventid_link_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " button click
  event-eventid = cl_gui_list_tree=>eventid_button_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " checkbox change
  event-eventid = cl_gui_list_tree=>eventid_checkbox_change.
  event-appl_event = 'X'.
  APPEND event TO events.

  CALL METHOD g_tree->set_registered_events
    EXPORTING
      events                    = events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE a000(tree_control_msg).
  ENDIF.

* assign event handlers in the application class to each desired event
  SET HANDLER g_application->handle_node_double_click FOR g_tree.
  SET HANDLER g_application->handle_item_double_click FOR g_tree.
  SET HANDLER g_application->handle_expand_no_children FOR g_tree.
  SET HANDLER g_application->handle_link_click FOR g_tree.
  SET HANDLER g_application->handle_button_click FOR g_tree.
  SET HANDLER g_application->handle_checkbox_change FOR g_tree.

*  SET HANDLER g_application->TOP_OF_PAGE FOR g_tree. "aqui

ENDFORM.                    " CREATE_AND_INIT_TREE

*&---------------------------------------------------------------------*
*&      Form  BUILD_NODE_AND_ITEM_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_TABLE  text
*      -->P_ITEM_TABLE  text
*----------------------------------------------------------------------*
FORM build_node_and_item_table  USING node_table TYPE treev_ntab
                                      item_table TYPE item_table_type.

  DATA: node             TYPE treev_node,
        item             TYPE mtreeitm,
        it_zfit182_cab   TYPE TABLE OF zfit182 WITH HEADER LINE,
        it_zfit182_det02 TYPE TABLE OF zfit182 WITH HEADER LINE,
        it_zfit182_det03 TYPE TABLE OF zfit182 WITH HEADER LINE,
        it_zfit182_det04 TYPE TABLE OF zfit182 WITH HEADER LINE,
        it_zfit182_det05 TYPE TABLE OF zfit182 WITH HEADER LINE,
        it_zfit182_det06 TYPE TABLE OF zfit182 WITH HEADER LINE,
        it_zfit183_aux   TYPE TABLE OF zfit183 WITH HEADER LINE,
        it_zfit184_aux   TYPE TABLE OF zfit184 WITH HEADER LINE,
        it_zfit182_txt   TYPE TABLE OF zfit182 WITH HEADER LINE,
        xid              TYPE string.

  CLEAR: node_table,
         item_table,
         it_zfit182[],
         it_zfit182_cab[],
         it_zfit183[],
         it_zfit183_aux[],
         it_zfit183_id[],

         it_zfit184[],
         it_zfit184_aux[],
         it_zfit184_id[].


  "***  Empresa  - Saldo da aplicação ****************************************************
  SELECT * INTO TABLE it_zfit184
    FROM zfit184
   WHERE bukrs EQ wa_zfit181-bukrs
     AND cod_estrutura EQ wa_zfit181-cod_estrutura.

  MOVE it_zfit184[] TO it_zfit184_aux[].
  SORT it_zfit184_aux BY empresa.
  DELETE ADJACENT DUPLICATES FROM it_zfit184_aux COMPARING empresa.

  SORT it_zfit184 BY nivel empresa descricao.

  LOOP AT it_zfit184.
    MOVE-CORRESPONDING it_zfit184 TO it_zfit184_id.
*    MOVE sy-tabix TO xid.
    MOVE it_zfit184-empresa TO xid.
    CONCATENATE 'CC_' xid INTO it_zfit184_id-id.

    it_zfit184_id-tx = it_zfit184-descricao.

    APPEND it_zfit184_id.
  ENDLOOP.


  "*************************************************************************************

**  Begin of CS2022000708  #83806 FF   12.12.2022 14:47:57
**  "*************************************************************************************
  "*** Código de fluxo de caixa *********************************************************************
  SELECT * INTO TABLE it_zfit183
    FROM zfit183
   WHERE bukrs EQ wa_zfit181-bukrs
     AND cod_estrutura EQ wa_zfit181-cod_estrutura.

  MOVE it_zfit183[] TO it_zfit183_aux[].
  SORT it_zfit183_aux BY codigo_fluxo descricao.
  DELETE ADJACENT DUPLICATES FROM it_zfit183_aux COMPARING codigo_fluxo descricao.

  SORT it_zfit183 BY nivel codigo_fluxo.

  LOOP AT it_zfit183.
    MOVE-CORRESPONDING it_zfit183 TO it_zfit183_id.
*    MOVE sy-tabix TO xid.
*    MOVE it_zfit183-codigo_fluxo TO xid.
    CONCATENATE it_zfit183-codigo_fluxo it_zfit183-nivel(5) INTO xid.
    CONCATENATE 'CR_' xid INTO it_zfit183_id-id.

    it_zfit183_id-tx = it_zfit183-descricao.

    APPEND it_zfit183_id.
  ENDLOOP.
*************************************************************************************

** End of FF  12.12.2022 14:47:57

  SELECT * INTO TABLE it_zfit182
    FROM zfit182
   WHERE bukrs EQ wa_zfit181-bukrs
     AND cod_estrutura EQ wa_zfit181-cod_estrutura.

  IF sy-subrc IS INITIAL.

    SORT it_zfit182 BY item_estrutura ASCENDING.

    MOVE it_zfit182[] TO it_zfit182_cab[].
    DELETE it_zfit182_cab WHERE nivel_ant NE space.

    LOOP AT it_zfit182_cab.

      CLEAR: node-relatkey,    "Special case: A root node has no parent
             node-relatship,   "node.
             node-expander.    " see below

*      IF it_zfit182_cab-nivel_total IS INITIAL.
      node-n_image   = icon_next_node.
      node-exp_image = icon_previous_node.
*      ELSE.
*        node-n_image   = icon_sum.
*        node-exp_image = icon_sum.
*      ENDIF.
      node-hidden    = ' '.    " The node is visible,
      node-disabled  = ' '.    " selectable,
      node-isfolder  = 'X'.    " a folder.
      node-node_key  = it_zfit182_cab-nivel.
**  Begin of CS2022000708  #83806 FF   14.12.2022 20:51:58
      IF it_zfit182_cab-nivel = 'FD'.
        CONCATENATE  it_zfit182_cab-nivel '_' it_zfit182_cab-item_estrutura  INTO node-node_key.
      ENDIF.
** End of FF  14.12.2022 20:51:58

**  Begin of CS2022000708  #115538 RJF   29.06.2023
      IF it_zfit182_cab-nivel = 'EN.FD' OR it_zfit182_cab-nivel = 'SA.FD' OR it_zfit182_cab-nivel = 'ZE.FD'.
        CONCATENATE  it_zfit182_cab-nivel '_' it_zfit182_cab-item_estrutura  INTO node-node_key.
      ENDIF.
** End of RJF  29.06.2023

      REPLACE ALL OCCURRENCES OF REGEX '[.]' IN node-node_key WITH ''.
      APPEND node TO node_table.

      CLEAR item.
      item-node_key   = node-node_key.
      item-item_name  = '1'."p_node-node_key. " Item with name '1'
      item-class      = cl_gui_list_tree=>item_class_text. " Text Item
      item-alignment  = cl_gui_list_tree=>align_auto.
      item-font       = cl_gui_list_tree=>item_font_prop.
      item-text       = it_zfit182_cab-nivel_text. "CS2022000708  #83806 FF   15.12.2022 12:01:13

      "item-usebgcolor = 'X'.
      APPEND item TO item_table.

*      item-item_name = '2'."p_node-node_key. " Item with name '2'
*      item-text      = it_zfit182_cab-nivel_total.
*      APPEND item TO item_table.

      item-item_name = '2'."p_node-node_key. " Item with name '3'
      item-text      = it_zfit182_cab-nitxt.
      APPEND item TO item_table.


      MOVE it_zfit182[] TO it_zfit182_det02[].
      READ TABLE it_zfit182_det02 WITH KEY nivel = it_zfit182_cab-nivel
                                                    nivel_ant = space.
      IF sy-subrc IS INITIAL.
        PERFORM add_node USING node_table item_table c_x it_zfit182_det02 node item c_x.
      ENDIF.

      DELETE it_zfit182_det02 WHERE nivel_ant NE it_zfit182_cab-nivel.

      LOOP AT it_zfit182_det02.

        CLEAR: it_zfit182_det03[].
        MOVE it_zfit182[] TO it_zfit182_det03[].
        DELETE it_zfit182_det03 WHERE nivel_ant NE it_zfit182_det02-nivel.

        "Criar Filho do nó
        IF NOT it_zfit182_det03[] IS INITIAL.
          PERFORM add_node USING node_table item_table c_x it_zfit182_det02 node item space.
        ELSE.
          PERFORM add_node USING node_table item_table space it_zfit182_det02 node item space.
        ENDIF.

        LOOP AT it_zfit182_det03.
          CLEAR: it_zfit182_det04[].
          MOVE it_zfit182[] TO it_zfit182_det04[].
          DELETE it_zfit182_det04 WHERE nivel_ant NE it_zfit182_det03-nivel.

          "Criar Filho do nó
          IF NOT it_zfit182_det04[] IS INITIAL.
            PERFORM add_node USING node_table item_table c_x it_zfit182_det03 node item space.
          ELSE.
            PERFORM add_node USING node_table item_table space it_zfit182_det03 node item space.
          ENDIF.

          LOOP AT it_zfit182_det04.
            CLEAR: it_zfit182_det05[].
            MOVE it_zfit182[] TO it_zfit182_det05[].
            DELETE it_zfit182_det05 WHERE nivel_ant NE it_zfit182_det04-nivel.
            "Criar Filho do nó
            IF NOT it_zfit182_det05[] IS INITIAL.
              PERFORM add_node USING node_table item_table c_x it_zfit182_det04 node item space.
            ELSE.
              PERFORM add_node USING node_table item_table space it_zfit182_det04 node item space.
            ENDIF.

            LOOP AT it_zfit182_det05.
              CLEAR: it_zfit182_det06[].
              "Criar Filho do nó
              PERFORM add_node USING node_table item_table space it_zfit182_det05 node item space.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " BUILD_NODE_AND_ITEM_TABLE


*&---------------------------------------------------------------------*
*&      Form  ADD_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_node  USING  node_table                  TYPE treev_ntab
                      item_table                  TYPE item_table_type
                      p_existe_sub                TYPE c
                      p_it_zfit182_detat TYPE zfit182
                      p_node                      TYPE treev_node
                      p_item                      TYPE mtreeitm
                      p_objt                      TYPE c.

  DATA: vg_nivel01 TYPE znivel_dre,
        vg_nivel02 TYPE znivel_dre.

  CLEAR: p_node-relatkey,    "Special case: A root node has no parent
         p_node-relatship,   "node.
         p_node-expander.    " see below

  vg_nivel01 = p_it_zfit182_detat-nivel.
  vg_nivel02 = p_it_zfit182_detat-nivel_ant.

  REPLACE ALL OCCURRENCES OF REGEX '[.]' IN vg_nivel01 WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[.]' IN vg_nivel02 WITH ''.

  IF p_objt IS INITIAL.

    IF NOT p_existe_sub IS INITIAL.
      p_node-n_image   = icon_next_node.
      p_node-exp_image = icon_previous_node.
    ELSE.
      READ TABLE it_zfit183_id WITH KEY nivel = p_it_zfit182_detat-nivel.
      IF sy-subrc IS INITIAL.
        p_node-n_image   = icon_next_node.
        p_node-exp_image = icon_tree.
      ELSE.
        p_node-n_image   = icon_led_red.
        p_node-exp_image = icon_led_red.
      ENDIF.
    ENDIF.
    p_node-isfolder  = 'X'.    " a folder.
    p_node-hidden    = ' '.    " The node is visible,
    p_node-disabled  = ' '.    " selectable,
    p_node-node_key  = vg_nivel01.
    p_node-relatkey  = vg_nivel02.
    p_node-relatship = cl_gui_list_tree=>relat_last_child.
    APPEND p_node TO node_table.

    CLEAR p_item.
    p_item-node_key   = p_node-node_key.
    p_item-item_name  = '1'."p_node-node_key. " Item with name '1'
    p_item-class      = cl_gui_list_tree=>item_class_text. " Text Item
    p_item-alignment  = cl_gui_list_tree=>align_auto.
    p_item-font       = cl_gui_list_tree=>item_font_prop.
    p_item-text       = p_it_zfit182_detat-nivel_text. "p_it_zfit182_detat-nivel. "CS2022000708  #83806 FF   15.12.2022 12:01:13
    "p_item-usebgcolor = 'X'.
    APPEND p_item TO item_table.

*    p_item-item_name  = '2'."p_node-node_key. " Item with name '2'
*    p_item-text       = p_it_zfit182_detat-nivel_total.
*    APPEND p_item TO item_table.

    p_item-item_name  = '2'."p_node-node_key. " Item with name '3'
    p_item-text       = p_it_zfit182_detat-nitxt.
    APPEND p_item TO item_table.


  ENDIF.

  LOOP AT it_zfit183_id WHERE nivel EQ p_it_zfit182_detat-nivel.

    p_node-n_image   = icon_subscription.
    p_node-exp_image = icon_outbox.
    p_node-isfolder  = ' '.    " a folder.
**  Begin of CS2022000708  #83806 FF   12.12.2022 16:06:38
    READ TABLE it_zfit184_id WITH KEY nivel = it_zfit183_id-nivel.
*                                               saknr = it_zfit183_id-saknr.
    IF sy-subrc IS INITIAL.
      p_node-isfolder  = 'X'.    " a folder.
    ENDIF.
*    READ TABLE it_zgl015_dre_est05_id WITH KEY nivel = it_zfit183_id-nivel
*                                               saknr = it_zfit183_id-saknr.
*    IF sy-subrc IS INITIAL.
*      p_node-isfolder  = 'X'.    " a folder.
*    ENDIF.
*    READ TABLE it_zgl015_dre_est06_id WITH KEY nivel = it_zfit183_id-nivel
*                                               saknr = it_zfit183_id-saknr.
*    IF sy-subrc IS INITIAL.
*      p_node-isfolder  = 'X'.    " a folder.
*    ENDIF.

** End of FF  12.12.2022 16:06:38
    p_node-hidden    = ' '.    " The node is visible,
    p_node-disabled  = ' '.    " selectable,
    p_node-node_key  = it_zfit183_id-id.
    p_node-relatkey  = vg_nivel01.
    p_node-relatship = cl_gui_list_tree=>relat_last_child.
    APPEND p_node TO node_table.

    CLEAR p_item.
    p_item-node_key   = p_node-node_key.
    p_item-item_name  = '1'."p_node-node_key. " Item with name '1'
    p_item-class      = cl_gui_list_tree=>item_class_text. " Text Item
    p_item-alignment  = cl_gui_list_tree=>align_auto.
    p_item-font       = cl_gui_list_tree=>item_font_prop.
    p_item-text       = it_zfit183_id-codigo_fluxo. " CS2022000708  #83806 FF   12.12.2022 16:07:28

    APPEND p_item TO item_table.

    p_item-item_name  = '2'."p_node-node_key. " Item with name '1'
    p_item-text       = it_zfit183_id-descricao . " CS2022000708  #83806 FF   12.12.2022 16:07:28
    APPEND p_item TO item_table.

**  Begin of CS2022000708  #83806 FF   14.12.2022 15:20:51
*    p_item-item_name  = '3'."p_node-node_key. " Item with name '1'
*    p_item-text       = it_zfit183_id-descricao.
*    APPEND p_item TO item_table.
** End of FF  14.12.2022 15:20:51

*    IF it_zfit183_id-levar_qtde IS NOT INITIAL.
*      p_item-item_name = '4'."p_node-node_key. " Item with name '1'
*      p_item-text      = '(Soma Quantidades)'.
*      APPEND p_item TO item_table.
*    ENDIF.

*ICON_BIW_APPLICATION
    LOOP AT it_zfit184_id WHERE nivel EQ p_it_zfit182_detat-nivel.
      " AND saknr EQ it_zfit183_id-saknr.

      p_node-n_image   = icon_biw_application.
      p_node-exp_image = icon_biw_application.
      p_node-isfolder  = ' '.    " a folder.
      p_node-hidden    = ' '.    " The node is visible,
      p_node-disabled  = ' '.    " selectable,
      p_node-node_key  = it_zfit184_id-id.
      p_node-relatkey  = vg_nivel01. "it_zfit183_id-id. *CS2022000708  #83806 FF   14.12.2022 12:13:55
      p_node-relatship = cl_gui_list_tree=>relat_last_child.
      APPEND p_node TO node_table.

      CLEAR p_item.
      p_item-node_key   = p_node-node_key.
      p_item-item_name  = '1'."p_node-node_key. " Item with name '1'
      p_item-class      = cl_gui_list_tree=>item_class_text. " Text Item
      p_item-alignment  = cl_gui_list_tree=>align_auto.
      p_item-font       = cl_gui_list_tree=>item_font_prop.
      p_item-text       = it_zfit184_id-descricao.
      APPEND p_item TO item_table.

      p_item-item_name  = '2'."p_node-node_key. " Item with name '1'
      p_item-text       = it_zfit184_id-tx.
      APPEND p_item TO item_table.

    ENDLOOP.
  ENDLOOP.

**  Begin of CS2022000708  #83806 FF   14.12.2022 13:51:35
*ICON_BIW_APPLICATION
  IF vg_nivel01 = 'S+'.
    LOOP AT it_zfit184_id WHERE nivel EQ p_it_zfit182_detat-nivel.
      " AND saknr EQ it_zfit183_id-saknr.

      p_node-n_image   = icon_biw_application.
      p_node-exp_image = icon_biw_application.
      p_node-isfolder  = ' '.    " a folder.
      p_node-hidden    = ' '.    " The node is visible,
      p_node-disabled  = ' '.    " selectable,
      p_node-node_key  = it_zfit184_id-id.
      p_node-relatkey  = vg_nivel01. "it_zfit183_id-id. *CS2022000708  #83806 FF   14.12.2022 12:13:55
      p_node-relatship = cl_gui_list_tree=>relat_last_child.
      APPEND p_node TO node_table.

      CLEAR p_item.
      p_item-node_key   = p_node-node_key.
      p_item-item_name  = '1'."p_node-node_key. " Item with name '1'
      p_item-class      = cl_gui_list_tree=>item_class_text. " Text Item
      p_item-alignment  = cl_gui_list_tree=>align_auto.
      p_item-font       = cl_gui_list_tree=>item_font_prop.
      p_item-text       = it_zfit184_id-empresa.
      APPEND p_item TO item_table.

      p_item-item_name  = '2'."p_node-node_key. " Item with name '1'
      p_item-text       = it_zfit184_id-tx.
      APPEND p_item TO item_table.

    ENDLOOP.
  ENDIF.
** End of FF  14.12.2022 13:51:35



ENDFORM. " ADD_NODE

*&---------------------------------------------------------------------*
*&      Module  STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9004 OUTPUT.

  SET PF-STATUS 'PF9004'.
  SET TITLEBAR 'TL9004'.

  IF vg_editar IS INITIAL.
    LOOP AT SCREEN.
      IF ( screen-name EQ 'WA_FLUXO_EST02-NV01' ) AND ( qt_niveis = 0 ).
        screen-input    = 1.
        screen-required = 1.
        MODIFY SCREEN.
      ELSEIF ( screen-name EQ 'WA_FLUXO_EST02-NV02' ) AND ( qt_niveis = 1 ).
        screen-input    = 1.
        screen-required = 1.
        MODIFY SCREEN.
      ELSEIF ( screen-name EQ 'WA_FLUXO_EST02-NV03' ) AND ( qt_niveis = 2 ).
        screen-input    = 1.
        screen-required = 1.
        MODIFY SCREEN.
      ELSEIF ( screen-name EQ 'WA_FLUXO_EST02-NV04' ) AND ( qt_niveis = 3 ).
        screen-input    = 1.
        screen-required = 1.
        MODIFY SCREEN.
      ELSEIF ( screen-name EQ 'WA_FLUXO_EST02-NV05' ) AND ( qt_niveis = 4 ).
        screen-input    = 1.
        screen-required = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ( wa_fluxo_est02-nv05(2) IS NOT INITIAL AND wa_fluxo_est02-nv05(2) = 'FD' AND qt_niveis = 4 ) OR
     ( wa_fluxo_est02-nv04(2) IS NOT INITIAL AND wa_fluxo_est02-nv04(2) = 'FD' AND qt_niveis = 3 ) OR
     ( wa_fluxo_est02-nv03(2) IS NOT INITIAL AND wa_fluxo_est02-nv03(2) = 'FD' AND qt_niveis = 2 ) OR
     ( wa_fluxo_est02-nv02(2) IS NOT INITIAL AND wa_fluxo_est02-nv02(2) = 'FD' AND qt_niveis = 1 ) OR
     ( wa_fluxo_est02-nv01(2) IS NOT INITIAL AND wa_fluxo_est02-nv01(2) = 'FD' AND qt_niveis = 0 ).

    LOOP AT SCREEN.
      IF screen-name EQ 'WA_FLUXO_EST02-NITXT'.
        screen-input  = 1. "Deixar usuário inserir
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name EQ 'WA_FLUXO_EST02-NITXT'.
        screen-input  = 0. "Não deixar usuário inserir
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF vg_editar IS NOT INITIAL.
    IF   wa_fluxo_est02-nv01 <> 'SI' AND
         wa_fluxo_est02-nv01 <> 'SF' AND
         wa_fluxo_est02-nv01 <> 'SC' AND
         wa_fluxo_est02-nv01 <> 'S+'.

      LOOP AT SCREEN.
        IF screen-name EQ 'WA_FLUXO_EST02-NITXT'.
          screen-input  = 1. "Deixar usuário inserir
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*** End of FF  12.12.2022 21:03:41


ENDMODULE.                 " STATUS_9004  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9004_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_9004_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9004 INPUT.

  DATA: vg_dub TYPE zfit182.

  IF wa_fluxo_est02-nv01 = 'SI'.
    wa_fluxo_est02-nitxt = 'Saldo Inicial'.
  ENDIF.

  IF wa_fluxo_est02-nv01 = 'SF'.
    wa_fluxo_est02-nitxt = 'Saldo final'.
  ENDIF.

  IF wa_fluxo_est02-nv01 = 'EN' AND wa_fluxo_est02-nv02 IS INITIAL.
    wa_fluxo_est02-nitxt = 'Entrada'.
  ENDIF.

  IF wa_fluxo_est02-nv01 = 'SA' AND wa_fluxo_est02-nv02 IS INITIAL.
    wa_fluxo_est02-nitxt = 'Saída'.
  ENDIF.

  IF wa_fluxo_est02-nv01 = 'ZE' AND wa_fluxo_est02-nv02 IS INITIAL.
    wa_fluxo_est02-nitxt = 'Zerado'.
  ENDIF.

  IF wa_fluxo_est02-nv01 = 'ST'.
    wa_fluxo_est02-nitxt = 'Saldo final total'.
  ENDIF.

  IF wa_fluxo_est02-nv01 = 'VC'
  OR wa_fluxo_est02-nv02 = 'VC'
  OR wa_fluxo_est02-nv03 = 'VC'
  OR wa_fluxo_est02-nv04 = 'VC'
  OR wa_fluxo_est02-nv05 = 'VC'.

    wa_fluxo_est02-nitxt = 'Valores de Cód. fluxo'.
  ENDIF.

  IF wa_fluxo_est02-nv01 = 'SC'.
    wa_fluxo_est02-nitxt = 'Aplicações / Sobra Caixa'.
  ENDIF.

  IF wa_fluxo_est02-nv01 = 'S+'.
    wa_fluxo_est02-nitxt = 'Saldo final + Outras aplicações'.
  ENDIF.

  IF ok_code_9004 EQ c_conf.

    IF wa_fluxo_est02-nv01 = 'VC'.
      MESSAGE '"VC" Só pode ser criado como filho do "FD" ' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

**  Begin of CS2022000708  #83806 FF   13.12.2022 10:29:37
    IF zfit182-nivel_ant IS NOT INITIAL.
*      IF zfit182-nivel_ant(2) = 'FD'.
      IF zfit182-nivel_ant(2) = 'FD' OR zfit182-nivel_ant(2) = 'EN' OR zfit182-nivel_ant(2) = 'SA' OR zfit182-nivel_ant(2) = 'ZE'.
        DATA: var_1(4), var_2(4), var_3(4).
*        DATA(lv_tam) = strlen( zfit182-nivel_ant ) - 4.
*        DATA(lv_nivel_ant) = zfit182-nivel_ant+lv_tam(4).
        SPLIT zfit182-nivel_ant AT '.' INTO var_1 var_2 var_3.
        IF var_3 IS NOT INITIAL.
          DATA(lv_nivel_ant) = var_3.
        ELSEIF var_2 IS NOT INITIAL.
          lv_nivel_ant = var_2.
        ELSE.
          lv_nivel_ant = var_1.
        ENDIF.
      ELSE.
        DATA(lv_tam)  = strlen( zfit182-nivel_ant ) - 2.
        lv_nivel_ant = zfit182-nivel_ant+lv_tam(2).

      ENDIF.

      IF wa_fluxo_est02-nv01 = 'VC' OR
        ( wa_fluxo_est02-nv02 = 'VC' AND lv_nivel_ant(2) <> 'FD' ) OR
        ( wa_fluxo_est02-nv03 = 'VC' AND lv_nivel_ant(2) <> 'FD' ) OR
        ( wa_fluxo_est02-nv04 = 'VC' AND lv_nivel_ant(2) <> 'FD' ) OR
        ( wa_fluxo_est02-nv05 = 'VC' AND lv_nivel_ant(2) <> 'FD' ).

        MESSAGE '"VC" Só pode ser criado como filho do "FD" ' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
** End of FF  13.12.2022 10:29:37

    CASE qt_niveis.
      WHEN 0.
        zfit182-nivel = wa_fluxo_est02-nv01.
      WHEN 1.
        CONCATENATE zfit182-nivel_ant '.' wa_fluxo_est02-nv02 INTO zfit182-nivel.
      WHEN 2.
        CONCATENATE zfit182-nivel_ant '.' wa_fluxo_est02-nv03 INTO zfit182-nivel.
      WHEN 3.
        CONCATENATE zfit182-nivel_ant '.' wa_fluxo_est02-nv04 INTO zfit182-nivel.
      WHEN 4.
        CONCATENATE zfit182-nivel_ant '.' wa_fluxo_est02-nv05 INTO zfit182-nivel.
    ENDCASE.

    IF vg_editar IS INITIAL.
      SELECT SINGLE * INTO @DATA(ls_data)
        FROM zfit182
       WHERE cod_estrutura EQ @zfit182-cod_estrutura
         AND nivel EQ @zfit182-nivel.

      IF sy-subrc IS INITIAL.
        MESSAGE 'Este nível já existe!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF wa_fluxo_est02-nitxt IS INITIAL.
      MESSAGE 'Nome do agrupador obrigatório' TYPE 'S'.
      EXIT.
    ENDIF.

    zfit182-nitxt       = wa_fluxo_est02-nitxt.

    IF vg_editar IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(ls_zfit182_aux)
        FROM zfit182
        WHERE cod_estrutura = @zfit182-cod_estrutura
          AND nivel         = @zfit182-nivel.
*          AND nitxt         = @zfit182-nitxt. " RJF

      IF sy-subrc = 0.

        zfit182-bukrs           = ls_zfit182_aux-bukrs.
        zfit182-cod_estrutura   = ls_zfit182_aux-cod_estrutura.
        zfit182-item_estrutura  = ls_zfit182_aux-item_estrutura.
        zfit182-nivel           = ls_zfit182_aux-nivel.
        zfit182-nivel_ant       = ls_zfit182_aux-nivel_ant.
        zfit182-nivel_text      = ls_zfit182_aux-nivel_text.

      ENDIF.

*    zfit182-item_estrutura  = lv_last_item + 1.
    ELSE.

      SELECT SINGLE MAX( item_estrutura ) INTO @DATA(ultimo_item_estrutura)
        FROM zfit182
        WHERE bukrs = @zfit182-bukrs
          AND cod_estrutura EQ @zfit182-cod_estrutura.

      IF sy-subrc <> 0.
        CLEAR ultimo_item_estrutura.
      ENDIF.

      zfit182-item_estrutura  = ultimo_item_estrutura + 1.
    ENDIF.

    DATA: var1(4),
          var2(4),
          var3(4),
          var4(4).
    SPLIT zfit182-nivel AT '.' INTO var1 var2 var3 var4.

    IF var4 IS NOT INITIAL.
      CONCATENATE var1(2) var2(2) var3(2) var4(2) INTO DATA(lv_nivel_text) SEPARATED BY '.'.
    ELSEIF var3 IS NOT INITIAL.
      CONCATENATE var1(2) var2(2) var3(2) INTO lv_nivel_text SEPARATED BY '.'.
    ELSEIF var2 IS NOT INITIAL.
      CONCATENATE var1(2) var2(2) INTO lv_nivel_text SEPARATED BY '.'.
    ELSE.
      lv_nivel_text  = var1(2).
    ENDIF.

    zfit182-nivel_text = lv_nivel_text.

    IF zfit182-nivel = 'FD'.
      CONCATENATE zfit182-nivel '_' zfit182-item_estrutura INTO zfit182-nivel.
    ENDIF.

**  Begin of CS2022000708  #115538 RJF   29.06.2023
    IF zfit182-nivel = 'EN.FD' OR zfit182-nivel = 'SA.FD' OR zfit182-nivel = 'ZE.FD'.
      CONCATENATE zfit182-nivel '_' zfit182-item_estrutura INTO zfit182-nivel.
    ENDIF.
** End of RJF  29.06.2023

    MODIFY zfit182.

    CALL METHOD g_tree->delete_all_nodes.
    prim_dre_nivel_re = c_x.
    PERFORM atualiza_alv_est.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_9004  INPUT


FORM copiar_est.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_zfit181_alv.

  CALL METHOD dre_alv_est->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF it_selected_rows IS INITIAL.
    MESSAGE 'Selecione uma estrutura!' TYPE 'S'.
  ELSE.
    READ TABLE it_selected_rows INDEX 1 INTO wa_selected_rows.
    READ TABLE it_zfit181_alv INTO wa_zfit181_alv INDEX wa_selected_rows-index.
    CALL SCREEN 9005 STARTING AT 10 10.
  ENDIF.

ENDFORM.                    " COPIAR_EST


FORM apagar_est.

  TABLES: zgl021_dre_dados, zgl022_dre_dados, zgl023_dre_dados, zgl024_dre_dados.
  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row,
        answer           TYPE c LENGTH 1.

  CLEAR: wa_zfit181_alv.

  CALL METHOD dre_alv_est->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF it_selected_rows IS INITIAL.
    MESSAGE 'Selecione uma estrutura!' TYPE 'S'.
  ELSE.
    READ TABLE it_selected_rows INDEX 1 INTO wa_selected_rows.
    READ TABLE it_zfit181_alv INTO wa_zfit181_alv INDEX wa_selected_rows-index.

    IF sy-subrc = 0.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = 'Atenção!'
          textline1 = 'A Estrutura será excluida!'
          textline2 = 'Deseja realmente excluir?'
        IMPORTING
          answer    = answer.

      IF answer EQ c_j.

        DELETE FROM zfit181
         WHERE bukrs EQ wa_zfit181_alv-bukrs
           AND cod_estrutura EQ wa_zfit181_alv-cod_estrutura.

        DELETE FROM zfit182
         WHERE bukrs EQ wa_zfit181_alv-bukrs
           AND cod_estrutura EQ wa_zfit181_alv-cod_estrutura.

        DELETE FROM zfit183
         WHERE bukrs EQ wa_zfit181_alv-bukrs
           AND cod_estrutura EQ wa_zfit181_alv-cod_estrutura.

        DELETE FROM zfit184
         WHERE bukrs EQ wa_zfit181_alv-bukrs
           AND cod_estrutura EQ wa_zfit181_alv-cod_estrutura.

        PERFORM atualiza_alv_est.

        MESSAGE 'Estrutura Excluida!' TYPE 'S'.
      ENDIF.
    ELSE.
      MESSAGE 'Estrutura não pode ser Excluida!' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.                    " APAGAR_EST

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ERROS_CL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_zfit181_VERSN  text
*----------------------------------------------------------------------*
FORM mostrar_erros_cl  USING p_versn TYPE versn_011.

*  DATA: error_text TYPE string,
*        exc_ref    TYPE REF TO cx_sy_native_sql_error,
*        wa_erro    TYPE ty_motra_erro.
*
*  TRY.
*      EXEC SQL.
*        OPEN REGISTROS_CL FOR
*          SELECT DR2.NIVEL, DR2.SAKNR, DR2.KOKRS, DR2.PRCTR
*          FROM (SELECT DR2.VERSN, DR2.SAKNR, DR2.KOKRS, DR2.PRCTR
*                   FROM SAPSR3.ZGL015_DRE_EST05 DR2
*                  WHERE VERSN = :P_VERSN
*                  GROUP BY DR2.VERSN, DR2.SAKNR, DR2.KOKRS, DR2.PRCTR
*                 HAVING COUNT(*) > 1 ) TT,
*                 SAPSR3.ZGL015_DRE_EST05 DR2
*          WHERE TT.VERSN  = DR2.VERSN
*             AND TT.SAKNR = DR2.SAKNR
*             AND TT.KOKRS = DR2.KOKRS
*             AND TT.PRCTR = DR2.PRCTR
*      ENDEXEC.
*    CATCH cx_sy_native_sql_error INTO exc_ref.
*      error_text = exc_ref->get_text( ).
*      MESSAGE error_text TYPE 'E'.
*  ENDTRY.
*
*  DO.
*    EXEC SQL.
*      FETCH NEXT REGISTROS_CL INTO
*      :WA_ERRO-NIVEL,
*      :WA_ERRO-SAKNR,
*      :WA_ERRO-KOKRS,
*      :WA_ERRO-PRCTR.
*    ENDEXEC.
*    IF sy-subrc <> 0.
*      EXIT.
*    ELSE.
*      APPEND wa_erro TO it_motra_erro.
*    ENDIF.
*  ENDDO.
*
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ERROS_CC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_zfit181_VERSN  text
*----------------------------------------------------------------------*
FORM mostrar_erros_cc  USING p_versn TYPE versn_011.

*  DATA: error_text TYPE string,
*        exc_ref    TYPE REF TO cx_sy_native_sql_error,
*        wa_erro    TYPE ty_motra_erro.
*
*  TRY.
*      EXEC SQL.
*        OPEN REGISTROS_CC FOR
*          SELECT DR2.NIVEL, DR2.SAKNR, DR2.KOSAR
*          FROM (SELECT DR2.VERSN, DR2.SAKNR, DR2.KOSAR
*                   FROM SAPSR3.zfit184 DR2
*                  WHERE VERSN = :P_VERSN
*                  GROUP BY DR2.VERSN, DR2.SAKNR, DR2.KOSAR
*                 HAVING COUNT(*) > 1 ) TT,
*                 SAPSR3.zfit184 DR2
*          WHERE TT.VERSN  = DR2.VERSN
*             AND TT.SAKNR = DR2.SAKNR
*             AND TT.KOSAR = DR2.KOSAR
*      ENDEXEC.
*    CATCH cx_sy_native_sql_error INTO exc_ref.
*      error_text = exc_ref->get_text( ).
*      MESSAGE error_text TYPE 'E'.
*  ENDTRY.
*
*  DO.
*    EXEC SQL.
*      FETCH NEXT REGISTROS_CC INTO
*      :WA_ERRO-NIVEL,
*      :WA_ERRO-SAKNR,
*      :WA_ERRO-KOSAR.
*    ENDEXEC.
*    IF sy-subrc <> 0.
*      EXIT.
*    ELSE.
*      APPEND wa_erro TO it_motra_erro.
*    ENDIF.
*  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ERROS_GM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_zfit181_VERSN  text
*----------------------------------------------------------------------*
FORM mostrar_erros_gm  USING p_versn TYPE versn_011.

*  DATA: error_text TYPE string,
*        exc_ref    TYPE REF TO cx_sy_native_sql_error,
*        wa_erro    TYPE ty_motra_erro.
*
*  TRY.
*      EXEC SQL.
*        OPEN REGISTROS_GM FOR
*          SELECT DR2.NIVEL, DR2.SAKNR, DR2.MATKL
*          FROM (SELECT DR2.VERSN, DR2.SAKNR, DR2.MATKL
*                   FROM SAPSR3.ZGL015_DRE_EST06 DR2
*                  WHERE VERSN = :P_VERSN
*                  GROUP BY DR2.VERSN, DR2.SAKNR, DR2.MATKL
*                 HAVING COUNT(*) > 1 ) TT,
*                 SAPSR3.ZGL015_DRE_EST06 DR2
*          WHERE TT.VERSN  = DR2.VERSN
*             AND TT.SAKNR = DR2.SAKNR
*             AND TT.MATKL = DR2.MATKL
*      ENDEXEC.
*    CATCH cx_sy_native_sql_error INTO exc_ref.
*      error_text = exc_ref->get_text( ).
*      MESSAGE error_text TYPE 'E'.
*  ENDTRY.
*
*  DO.
*    EXEC SQL.
*      FETCH NEXT REGISTROS_GM INTO
*      :WA_ERRO-NIVEL,
*      :WA_ERRO-SAKNR,
*      :WA_ERRO-MATKL.
*    ENDEXEC.
*    IF sy-subrc <> 0.
*      EXIT.
*    ELSE.
*      APPEND wa_erro TO it_motra_erro.
*    ENDIF.
*  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ERROS_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostrar_erros_objetos .

*  IF it_motra_erro[] IS INITIAL.
*    MESSAGE s040.
*  ENDIF.
*
*  CHECK it_motra_erro[] IS NOT INITIAL.
*
*  CALL SCREEN 9011 STARTING AT 05 05.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  Z_MATHCODE_SEQ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_mathcode_seq1 INPUT.

  PERFORM f4_val USING 'VG_SEQ1'
                       'VG_SEQ_DESC1'.

ENDMODULE.
MODULE z_mathcode_seq2 INPUT.

  PERFORM f4_val USING 'VG_SEQ2'
                       'VG_SEQ_DESC2'.

ENDMODULE.
MODULE z_mathcode_seq3 INPUT.

  PERFORM f4_val USING 'VG_SEQ3'
                       'VG_SEQ_DESC3'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F4_VAL
*&---------------------------------------------------------------------*
FORM f4_val  USING    p_cod TYPE help_info-dynprofld
                      p_desc TYPE help_info-dynprofld.

*====>  Tabelas internas
  DATA: BEGIN OF t_seq OCCURS 0,
          seq       TYPE zfit0109-seq,
          descricao TYPE zfit0109-descricao,
        END OF t_seq.

  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  SELECT  seq descricao
    FROM  zfit0109 INTO TABLE t_seq
    WHERE seq <> space.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'SEQ'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Código de fluxo de caixa'
        value_org       = 'S'
      TABLES
        value_tab       = t_seq
        return_tab      = t_return
        dynpfld_mapping = t_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_COD_FLUXO_CAIXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_SEQ1  text
*      -->P_VG_SEQ_DESC1  text
*----------------------------------------------------------------------*
FORM atualiza_cod_fluxo_caixa  USING    p_vg_seq
                                        p_vg_seq_desc.

  DATA: cod_fluxo_cadas TYPE char50.

  SELECT SINGLE * INTO @DATA(vg_cc)
    FROM zfit183
   WHERE bukrs EQ @zfit182-bukrs
     AND cod_estrutura EQ @zfit182-cod_estrutura
     AND nivel EQ @zfit182-nivel
     AND codigo_fluxo   EQ @p_vg_seq.

  IF sy-subrc IS INITIAL.

    IF p_vg_seq IS NOT INITIAL.
      SELECT SINGLE descricao
        FROM zfit0109
        INTO vg_seq_desc1
        WHERE codigo = p_vg_seq.
      IF sy-subrc <> 0.
        CLEAR p_vg_seq_desc.
        CONCATENATE 'Código' p_vg_seq 'inválido' INTO DATA(cod_fluxo_invalido) SEPARATED BY space.
        MESSAGE cod_fluxo_invalido TYPE 'E'.
      ENDIF.
    ENDIF.

    CLEAR: cod_fluxo_cadas.
    CONCATENATE 'Cód. fluxo de caixa já cadastrado!' p_vg_seq '-' p_vg_seq_desc INTO cod_fluxo_cadas SEPARATED BY space.
    MESSAGE cod_fluxo_cadas TYPE 'E'.
  ENDIF.

  zfit183-bukrs = zfit182-bukrs.
  zfit183-cod_estrutura = zfit182-cod_estrutura.
  zfit183-item_estrutura = zfit182-item_estrutura.
  zfit183-nivel = zfit182-nivel.
  zfit183-codigo_fluxo   = p_vg_seq.
  zfit183-descricao = p_vg_seq_desc.

  IF zfit183-codigo_fluxo IS NOT INITIAL AND zfit183-descricao IS INITIAL.
    SELECT SINGLE descricao
      FROM zfit0109
      INTO zfit183-descricao
      WHERE codigo = zfit183-codigo_fluxo.
    IF sy-subrc <> 0.
      CLEAR zfit183-descricao.
    ENDIF.
  ENDIF.

  INSERT zfit183.

  prim_dre_nivel_re = c_x.
  MOVE-CORRESPONDING zfit183 TO zfit182.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  Z_EDITAR_CAMPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_editar_campo INPUT.

**  Begin of CS2022000708  #83806 FF   12.12.2022 21:03:41
  IF wa_fluxo_est02-nv01 = 'FD'.
    LOOP AT SCREEN.
      IF screen-name EQ 'WA_FLUXO_EST02-NITXT'.
        screen-input  = 1. "Deixar usuário inserir
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
** End of FF  12.12.2022 21:03:41

ENDMODULE.
**&---------------------------------------------------------------------*
**&      Form  CHECK_COD_SEQ
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_VG_SEQ1  text
**----------------------------------------------------------------------*
*FORM f_check_cod_seq  USING    p_vg_seq.
*
*  CHECK p_vg_seq IS NOT INITIAL.
*
*  SELECT SINGLE *
*  FROM zfit0109
*  INTO @DATA(ls_zfit0109)
*  WHERE seq = @p_vg_seq.
*  IF sy-subrc <> 0.
*    CONCATENATE 'Código inválido -' p_vg_seq INTO DATA(cod_fluxo_invalido) SEPARATED BY space.
*    MESSAGE cod_fluxo_invalido TYPE 'E'." DISPLAY LIKE 'E'.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Module  Z_CHECK_CODE  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE z_check_code_seq1 INPUT.
*  PERFORM f_check_cod_seq USING vg_seq1.
*ENDMODULE.
*
*MODULE z_check_code_seq2 INPUT.
*  PERFORM f_check_cod_seq USING vg_seq2.
*ENDMODULE.
*
*MODULE z_check_code_seq3 INPUT.
*  PERFORM f_check_cod_seq USING vg_seq3.
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  Z_PREENCHE_NOME_EMPRESA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_preenche_nome_empresa INPUT.

  SELECT SINGLE butxt
    FROM t001
    INTO vg_bukrs_txt
    WHERE bukrs = zfit181-bukrs.
  IF sy-subrc <> 0.
    CLEAR vg_bukrs_txt.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DG_DYNDOC_ID  text
*----------------------------------------------------------------------*
FORM event_top_of_page USING dg_dyndoc_id TYPE REF TO cl_dd_document.
  "this is more clear.....check it
  "first add text, then pass it to comentry write fm
  DATA : dl_text(255) TYPE c.  "Text
* Populating header to top-of-page
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text      = 'Test Report'
      sap_style = cl_dd_area=>heading.
* Add new-line
  CALL METHOD dg_dyndoc_id->new_line.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  Z_MATHCODE_NIVEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_mathcode_nivel1 INPUT.
  PERFORM f4_val_nivel USING 'WA_FLUXO_EST02-NV01'
                             'WA_FLUXO_EST02-NITXT'.
ENDMODULE.
MODULE z_mathcode_nivel2 INPUT.
  PERFORM f4_val_nivel USING 'WA_FLUXO_EST02-NV02'
                             'WA_FLUXO_EST02-NITXT'.
ENDMODULE.
MODULE z_mathcode_nivel3 INPUT.
  PERFORM f4_val_nivel USING 'WA_FLUXO_EST02-NV03'
                             'WA_FLUXO_EST02-NITXT'.
ENDMODULE.
MODULE z_mathcode_nivel4 INPUT.
  PERFORM f4_val_nivel USING 'WA_FLUXO_EST02-NV04'
                             'WA_FLUXO_EST02-NITXT'.
ENDMODULE.
MODULE z_mathcode_nivel5 INPUT.
  PERFORM f4_val_nivel USING 'WA_FLUXO_EST02-NV05'
                             'WA_FLUXO_EST02-NITXT'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  F4_VAL_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4363   text
*----------------------------------------------------------------------*
FORM f4_val_nivel  USING    p_cod TYPE help_info-dynprofld
                            p_desc TYPE help_info-dynprofld.

*====>  Tabelas internas
  TYPES: BEGIN OF ty_nivel,
           nivel TYPE char4,
           desc  TYPE char50,
         END OF ty_nivel.

  DATA: t_nivel_f4 TYPE TABLE OF ty_nivel.

  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_nivel_f4   TYPE ty_nivel.
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  IF p_cod = 'WA_FLUXO_EST02-NV01'.
    s_nivel_f4-nivel = 'SI'.
    s_nivel_f4-desc = 'Saldo Inicial'.
    APPEND s_nivel_f4 TO t_nivel_f4.
  ENDIF.

* RJF - TST - Ini
  IF p_cod = 'WA_FLUXO_EST02-NV01'.
    s_nivel_f4-nivel = 'EN'.
    s_nivel_f4-desc = 'Entrada'.
    APPEND s_nivel_f4 TO t_nivel_f4.

    s_nivel_f4-nivel = 'SA'.
    s_nivel_f4-desc = 'Saída'.
    APPEND s_nivel_f4 TO t_nivel_f4.

    s_nivel_f4-nivel = 'ZE'.
    s_nivel_f4-desc = 'Zerado'.
    APPEND s_nivel_f4 TO t_nivel_f4.

    s_nivel_f4-nivel = 'ST'.
    s_nivel_f4-desc = 'Saldo Final Total'.
    APPEND s_nivel_f4 TO t_nivel_f4.

  ENDIF.
* RJF - TST - Fim

  s_nivel_f4-nivel = 'FD'.
  s_nivel_f4-desc = ' '.
  APPEND s_nivel_f4 TO t_nivel_f4.

  IF p_cod <> 'WA_FLUXO_EST02-NV01'.
    s_nivel_f4-nivel = 'VC'.
    s_nivel_f4-desc = 'Valores de Cód. Fluxo'.
    APPEND s_nivel_f4 TO t_nivel_f4.
  ENDIF.

  IF p_cod = 'WA_FLUXO_EST02-NV01'.
    s_nivel_f4-nivel = 'SC'.
    s_nivel_f4-desc = 'Aplicações / Sobra Caixa'.
    APPEND s_nivel_f4 TO t_nivel_f4.
  ENDIF.

  IF p_cod = 'WA_FLUXO_EST02-NV01'.
    s_nivel_f4-nivel = 'S+'.
    s_nivel_f4-desc = 'Saldo final + Outras aplicações'.
    APPEND s_nivel_f4 TO t_nivel_f4.

    s_nivel_f4-nivel = 'SF'.
    s_nivel_f4-desc = 'Saldo final'.
    APPEND s_nivel_f4 TO t_nivel_f4.
  ENDIF.

  s_mapping-fldname     = 'F0001'. "
  s_mapping-dyfldname   = p_cod.
  APPEND s_mapping TO t_mapping.
  CLEAR s_mapping.

  s_mapping-fldname     = 'F0002'. "
  s_mapping-dyfldname   = p_desc.
  APPEND s_mapping TO t_mapping.
  CLEAR s_mapping.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SEQ'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Código dos Níveis'
      value_org       = 'S'
    TABLES
      value_tab       = t_nivel_f4
      return_tab      = t_return
      dynpfld_mapping = t_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  Z_CHECK_CODE_NIVEL1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_COD_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FLUXO_EST02_NV01  text
*----------------------------------------------------------------------*
FORM f_check_cod_nivel  USING    p_nivel
                                 p_nivel_text.

  CHECK p_nivel IS NOT INITIAL.

  IF p_nivel(2) <> 'SI' AND
     p_nivel(2) <> 'FD' AND
     p_nivel(2) <> 'VC' AND
     p_nivel(2) <> 'SC' AND
     p_nivel(2) <> 'EN' AND " RJF
     p_nivel(2) <> 'SA' AND " RJF
     p_nivel(2) <> 'ZE' AND " RJF
     p_nivel(2) <> 'ST' AND " RJF
     p_nivel(2) <> 'S+' AND
     p_nivel(2) <> 'SF'.

    CONCATENATE 'Código' p_nivel(2) 'inválido'  INTO DATA(cod_nivel_invalido) SEPARATED BY space.

    CLEAR wa_fluxo_est02-nitxt.

    MESSAGE cod_nivel_invalido TYPE 'E'." DISPLAY LIKE 'E'.

  ENDIF.

  IF p_nivel_text = 'nivel_1' AND
    ( p_nivel(2) <> 'SI' AND
      p_nivel(2) <> 'FD' AND
      p_nivel(2) <> 'SC' AND
      p_nivel(2) <> 'EN' AND " RJF
      p_nivel(2) <> 'SA' AND " RJF
      p_nivel(2) <> 'ZE' AND " RJF
      p_nivel(2) <> 'ST' AND " RJF
      p_nivel(2) <> 'S+' AND
      p_nivel(2) <> 'SF' ).
    MESSAGE 'Código inválido para este nível' TYPE 'E'.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  Z_CHECK_CODE_NIVEL2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_check_code_nivel1 INPUT.
  PERFORM f_check_cod_nivel USING wa_fluxo_est02-nv01
                                  'nivel_1'.
ENDMODULE.

MODULE z_check_code_nivel2 INPUT.
  PERFORM f_check_cod_nivel USING wa_fluxo_est02-nv02
                                  'nivel_2'.
ENDMODULE.

MODULE z_check_code_nivel3 INPUT.
  PERFORM f_check_cod_nivel USING wa_fluxo_est02-nv03
                                  'nivel_3'.
ENDMODULE.

MODULE z_check_code_nivel4 INPUT.
  PERFORM f_check_cod_nivel USING wa_fluxo_est02-nv04
                                  'nivel_4'.
ENDMODULE.

MODULE z_check_code_nivel5 INPUT.
  PERFORM f_check_cod_nivel USING wa_fluxo_est02-nv05
                                  'nivel_5'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  Z_MATCHCODE_EMP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_matchcode_emp INPUT.
  PERFORM f4_val_emp USING 'ZFIT181-BUKRS'
                           'VG_BUKRS_TXT'.
ENDMODULE.
