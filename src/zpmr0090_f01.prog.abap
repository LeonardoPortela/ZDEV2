*&---------------------------------------------------------------------*
*& Include          ZPMR0090_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_seleciona_Dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_seleciona_Dados .

  IF p_cla IS NOT INITIAL AND
     ( p_class IS INITIAL OR p_dt_fim IS INITIAL OR p_dt_ini IS INITIAL ).
    MESSAGE 'Necessário preencher todos os campos!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF p_tp_ob IS NOT INITIAL AND
     ( s_eqart IS INITIAL OR p_dt_fim IS INITIAL OR p_dt_ini IS INITIAL ).
    MESSAGE 'Necessário preencher todos os campos!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF gt_class IS INITIAL.

    SELECT class kschg
      FROM m_clasa
      INTO TABLE gt_class
      WHERE spras = sy-langu
        AND klart = '002'.
    IF sy-subrc IS INITIAL.
      SORT gt_class BY class.
    ENDIF.

  ENDIF.

  IF p_class IS NOT INITIAL.

    READ TABLE gt_class ASSIGNING FIELD-SYMBOL(<fs_class>)
    WITH KEY class = p_class
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Classe inválida!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

  IF s_eqart[] IS NOT INITIAL.
    DATA(lt_eqart) = s_eqart[].
    READ TABLE lt_eqart ASSIGNING FIELD-SYMBOL(<fs_eqart1>) INDEX 1.
    IF sy-subrc IS INITIAL.

      IF <fs_eqart1>-high IS NOT INITIAL.
        DELETE lt_eqart WHERE low(1)  = <fs_eqart1>-low(1) AND
                              ( high(1) = <fs_eqart1>-high(1) AND
                                low(1) = <fs_eqart1>-high(1) ).
      ELSE.

        DELETE lt_eqart WHERE low(1)  = <fs_eqart1>-low(1).

      ENDIF.

      IF lt_eqart IS NOT INITIAL.
        MESSAGE 'Selecionar somente tipos de objetos da mesma classe' TYPE 'S' DISPLAY LIKE 'E'.

        FREE: s_eqart[].
        CLEAR s_eqart.
        EXIT.
      ENDIF.

    ENDIF.

  ENDIF.

  IF p_class IS NOT INITIAL.
    APPEND INITIAL LINE TO s_eqart ASSIGNING FIELD-SYMBOL(<fs_eqart>).
    <fs_eqart>-sign   = 'I'.
    <fs_eqart>-option = 'BT'.
    <fs_eqart>-low    = p_class.
    <fs_eqart>-high   = p_class + '99'.
    CONDENSE <fs_eqart>-high NO-GAPS.
  ENDIF.

  SELECT a~equnr a~eqart b~eqktx
    FROM equi AS a
    INNER JOIN eqkt AS b
    ON b~equnr = a~equnr
    INTO TABLE gt_equi
    WHERE a~eqart IN s_eqart
      AND b~spras EQ sy-langu.
  IF sy-subrc IS INITIAL.
    SORT gt_equi BY equnr.

    SELECT equnr iwerk iloan
      FROM equz
      INTO TABLE gt_equz
      FOR ALL ENTRIES IN gt_equi
      WHERE equnr = gt_equi-equnr
        AND datbi = '99991231'.
    IF sy-subrc IS INITIAL.
      SORT gt_equz BY equnr.

      DATA(lt_equz) = gt_equz.
      SORT lt_equz BY iloan.
      DELETE ADJACENT DUPLICATES FROM lt_equz COMPARING iloan.

      SELECT iloan adrnr
        FROM iloa
        INTO TABLE gt_iloa
        FOR ALL ENTRIES IN lt_equz
        WHERE iloan = lt_equz-iloan.
      IF sy-subrc IS INITIAL.
        SORT gt_iloa BY iloan.

        DATA(lt_iloa) = gt_iloa.
        SORT lt_iloa BY adrnr.
        DELETE ADJACENT DUPLICATES FROM lt_iloa COMPARING adrnr.

        SELECT addrnumber name1
          FROM adrc
          INTO TABLE gt_adrc
          FOR ALL ENTRIES IN lt_iloa
          WHERE addrnumber = lt_iloa-adrnr.
        IF sy-subrc IS INITIAL.
          SORT gt_adrc BY addrnumber.
        ENDIF.
      ENDIF.

    ENDIF.

    IF p_ence IS NOT INITIAL.
      SELECT equnr aufnr
        FROM afih
        INTO TABLE gt_afih
        FOR ALL ENTRIES IN gt_equi
        WHERE equnr = gt_equi-equnr.
    ELSE.
      SELECT equnr aufnr
        FROM afih
        INTO TABLE gt_afih
        FOR ALL ENTRIES IN gt_equi
        WHERE equnr = gt_equi-equnr
          AND iphas IN ( '0','1','2' ).
    ENDIF.

    IF sy-subrc IS INITIAL.
      SORT gt_afih BY aufnr.

      DATA(lt_afih) = gt_afih.
      SORT lt_afih BY aufnr.
      DELETE ADJACENT DUPLICATES FROM lt_afih COMPARING aufnr.

      SELECT *
        FROM zpmt0080
        INTO TABLE gt_zpmt0080
        FOR ALL ENTRIES IN lt_afih
        WHERE ordem_revisao = lt_afih-aufnr
          AND data_inicial >= p_dt_ini
          AND data_final   <= p_dt_fim.
      IF sy-subrc IS INITIAL.
        SORT gt_zpmt0080 BY ordem_revisao frota.
      ENDIF.

      SELECT *
        FROM aufk
        INTO TABLE gt_aufk
        FOR ALL ENTRIES IN lt_afih
        WHERE aufnr = lt_afih-aufnr
          AND auart = 'ZPM7'.
      IF sy-subrc IS INITIAL.
        SORT gt_aufk BY aufnr.

        SELECT aufnr gstrp gltrp aufpl
          FROM afko
          INTO TABLE gt_afko
          FOR ALL ENTRIES IN gt_aufk
          WHERE aufnr = gt_aufk-aufnr.
        IF sy-subrc IS INITIAL.
          SORT gt_afko BY aufnr.

          IF p_dt_ini IS NOT INITIAL.
            DELETE gt_afko WHERE gstrp < p_dt_ini.
          ENDIF.

          IF p_dt_fim IS NOT INITIAL.
            DELETE gt_afko WHERE gltrp > p_dt_fim.
          ENDIF.

          IF gt_afko IS NOT INITIAL.
            DATA(lt_afko) = gt_afko.
            SORT lt_afko BY aufpl.
            DELETE ADJACENT DUPLICATES FROM lt_afko COMPARING aufpl.

            SELECT aufpl objnr
              FROM afvc
              INTO TABLE gt_afvc
              FOR ALL ENTRIES IN lt_afko
              WHERE aufpl = lt_afko-aufpl.
            IF sy-subrc IS INITIAL.
              SORT gt_afvc BY aufpl.

              DATA(lt_afvc) = gt_afvc.
              SORT lt_afvc BY objnr.
              DELETE ADJACENT DUPLICATES FROM lt_afvc COMPARING objnr.

              SELECT kokrs belnr buzei objnr wtgbtr
                FROM coep
                INTO TABLE gt_coep
                FOR ALL ENTRIES IN lt_afvc
                WHERE objnr = lt_afvc-objnr
                  AND vrgng = 'COIN'.
              IF sy-subrc IS INITIAL.
                SORT gt_coep BY objnr.
              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  IF gt_afko IS INITIAL OR gt_equi IS INITIAL OR gt_afih IS INITIAL.
    MESSAGE 'Nenhum registro encontrado' TYPE 'S' DISPLAY LIKE 'E'.
    FREE: gt_equi,
          gt_afih,
          gt_aufk.
  ENDIF.

  SELECT kurztext
    FROM qpct
    INTO TABLE gt_qpct
    WHERE katalogart = 'D'
      AND codegruppe = 'ADERPLAN'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_dados .

  DATA: ls_header TYPE alm_me_tob_header,
        lt_costs  TYPE TABLE OF bapi_alm_order_costs_sum_e,
        lt_return TYPE TABLE OF bapiret2.

  LOOP AT gt_aufk ASSIGNING FIELD-SYMBOL(<fs_aufk>).

    READ TABLE gt_afih ASSIGNING FIELD-SYMBOL(<fs_afih1>)
    WITH KEY aufnr = <fs_aufk>-aufnr
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_zpmt0080 ASSIGNING FIELD-SYMBOL(<fs_zpmt0080>)
      WITH KEY ordem_revisao = <fs_afih1>-aufnr
               frota         = <fs_afih1>-equnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        APPEND INITIAL LINE TO gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
        MOVE-CORRESPONDING <fs_zpmt0080> TO <fs_saida>.
        <fs_saida>-estimado      = <fs_aufk>-user4.

        READ TABLE gt_afko ASSIGNING FIELD-SYMBOL(<fs_afko>)
        WITH KEY aufnr = <fs_aufk>-aufnr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          READ TABLE gt_afvc TRANSPORTING NO FIELDS
          WITH KEY aufpl = <fs_afko>-aufpl
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            LOOP AT gt_afvc ASSIGNING FIELD-SYMBOL(<fs_afvc>) FROM sy-tabix.
              IF <fs_afko>-aufpl <> <fs_afvc>-aufpl.
                EXIT.
              ENDIF.

              READ TABLE gt_coep TRANSPORTING NO FIELDS
              WITH KEY objnr = <fs_afvc>-objnr
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                LOOP AT gt_coep ASSIGNING FIELD-SYMBOL(<fs_coep>) FROM sy-tabix.
                  IF <fs_coep>-objnr <> <fs_afvc>-objnr.
                    EXIT.
                  ENDIF.

                  <fs_saida>-custo_total = <fs_saida>-custo_total + <fs_coep>-wtgbtr.

                ENDLOOP.

              ENDIF.

            ENDLOOP.

          ENDIF.

          IF <fs_saida>-estimado > 0.
            <fs_saida>-gasto = ( <fs_saida>-custo_total * 100 ) / <fs_saida>-estimado.
          ENDIF.

        ENDIF.

      ELSE.

        READ TABLE gt_afko ASSIGNING <fs_afko>
        WITH KEY aufnr = <fs_aufk>-aufnr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          APPEND INITIAL LINE TO gt_saida ASSIGNING <fs_saida>.

          <fs_saida>-drop_down_handle = '1'.
          <fs_saida>-estimado      = <fs_aufk>-user4.
          <fs_saida>-ordem_revisao = <fs_aufk>-aufnr.

          <fs_saida>-data_inicial = <fs_afko>-gstrp.
          <fs_saida>-data_final   = <fs_afko>-GlTRP.

          READ TABLE gt_afih ASSIGNING FIELD-SYMBOL(<fs_afih>)
          WITH KEY aufnr = <fs_aufk>-aufnr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-frota = <fs_afih>-equnr.

            READ TABLE gt_equi ASSIGNING FIELD-SYMBOL(<fs_equi>)
            WITH KEY equnr = <fs_afih>-equnr
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_Saida>-modelo = <fs_equi>-eqktx.
            ENDIF.

            READ TABLE gt_equz ASSIGNING FIELD-SYMBOL(<fs_equz>)
            WITH KEY equnr = <fs_equi>-equnr
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_saida>-centro = <fs_equz>-iwerk.

              READ TABLE gt_iloa ASSIGNING FIELD-SYMBOL(<fs_iloa>)
              WITH KEY iloan = <fs_equz>-iloan
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                READ TABLE gt_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc>)
                WITH KEY addrnumber = <fs_iloa>-adrnr
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  <fs_saida>-propriedade = <fs_adrc>-name1.
                ENDIF.

              ENDIF.

            ENDIF.

            READ TABLE gt_afvc TRANSPORTING NO FIELDS
            WITH KEY aufpl = <fs_afko>-aufpl
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              LOOP AT gt_afvc ASSIGNING <fs_afvc> FROM sy-tabix.
                IF <fs_afko>-aufpl <> <fs_afvc>-aufpl.
                  EXIT.
                ENDIF.

                READ TABLE gt_coep TRANSPORTING NO FIELDS
                WITH KEY objnr = <fs_afvc>-objnr
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.

                  LOOP AT gt_coep ASSIGNING <fs_coep> FROM sy-tabix.
                    IF <fs_coep>-objnr <> <fs_afvc>-objnr.
                      EXIT.
                    ENDIF.

                    <fs_saida>-custo_total = <fs_saida>-custo_total + <fs_coep>-wtgbtr.

                  ENDLOOP.

                ENDIF.

              ENDLOOP.

            ENDIF.

            IF <fs_saida>-estimado > 0.
              <fs_saida>-gasto = ( <fs_saida>-custo_total * 100 ) / <fs_saida>-estimado.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_alv .
  DATA: ls_layout    TYPE lvc_s_layo,
        lt_fieldcat  TYPE lvc_t_fcat,
        lv_classe    TYPE equi-eqart,
        lt_f4        TYPE lvc_t_f4,
        lw_f4        TYPE lvc_s_f4,
        lo_event_obj TYPE REF TO gcl_alv_event,
        lt_dropdown  TYPE lvc_t_drop,
        ls_dropdown  TYPE lvc_s_drop.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZPME_ALV_CALENDARIO'
    CHANGING
      ct_fieldcat      = lt_fieldcat.

  IF p_class IS NOT INITIAL.
    lv_classe = p_class.
  ELSE.
    lv_classe = s_eqart-low.
  ENDIF.

  LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

    CASE <fs_fieldcat>-fieldname.
      WHEN 'FROTA'.
        <fs_fieldcat>-scrtext_s = 'Frota'.
        <fs_fieldcat>-scrtext_m = 'Frota'.
        <fs_fieldcat>-scrtext_l = 'Frota'.
        <fs_fieldcat>-reptext   = 'Frota'.
      WHEN 'MODELO'.
        <fs_fieldcat>-scrtext_s = 'Modelo'.
        <fs_fieldcat>-scrtext_m = 'Modelo'.
        <fs_fieldcat>-scrtext_l = 'Modelo'.
        <fs_fieldcat>-reptext   = 'Modelo'.
      WHEN 'CENTRO'.
        <fs_fieldcat>-scrtext_s = 'Centro'.
        <fs_fieldcat>-scrtext_m = 'Centro'.
        <fs_fieldcat>-scrtext_l = 'Centro'.
        <fs_fieldcat>-reptext   = 'Centro'.
        <fs_fieldcat>-outputlen = 8.
      WHEN 'PROPRIEDADE'.
        <fs_fieldcat>-scrtext_s = 'Propriedade'.
        <fs_fieldcat>-scrtext_m = 'Propriedade'.
        <fs_fieldcat>-scrtext_l = 'Propriedade'.
        <fs_fieldcat>-reptext   = 'Propriedade'.
      WHEN 'STATUS'.
        <fs_fieldcat>-scrtext_s  = 'Status'.
        <fs_fieldcat>-scrtext_m  = 'Status'.
        <fs_fieldcat>-scrtext_l  = 'Status'.
        <fs_fieldcat>-reptext    = 'Status'.
        <fs_fieldcat>-edit = abap_true.
        <fs_fieldcat>-f4availabl = abap_true.
        <fs_fieldcat>-outputlen = 20.
      WHEN 'LOCALIZACAO'.
        <fs_fieldcat>-scrtext_s = 'Localização'.
        <fs_fieldcat>-scrtext_m = 'Localização'.
        <fs_fieldcat>-scrtext_l = 'Localização'.
        <fs_fieldcat>-reptext   = 'Localização'.
        <fs_fieldcat>-edit = abap_true.
        <fs_fieldcat>-drdn_field = 'DROP_DOWN_HANDLE'.
        <fs_fieldcat>-drdn_alias = 'X'.
        <fs_fieldcat>-outputlen = 20.
      WHEN 'ORDEM_REVISAO'.
        <fs_fieldcat>-scrtext_s = 'Ordem Revisão'.
        <fs_fieldcat>-scrtext_m = 'Ordem Revisão'.
        <fs_fieldcat>-scrtext_l = 'Ordem Revisão'.
        <fs_fieldcat>-reptext   = 'Ordem Revisão'.
      WHEN 'TOTAL'.
        <fs_fieldcat>-scrtext_s = 'Total'.
        <fs_fieldcat>-scrtext_m = 'Total'.
        <fs_fieldcat>-scrtext_l = 'Total'.
        <fs_fieldcat>-reptext   = 'Total'.
      WHEN 'ESTIMADO'.
        <fs_fieldcat>-scrtext_s = 'Vlr. Estimado'.
        <fs_fieldcat>-scrtext_m = 'Vlr. Estimado'.
        <fs_fieldcat>-scrtext_l = 'Vlr. Estimado'.
        <fs_fieldcat>-reptext = 'Vlr. Estimado'.
      WHEN 'CUSTO_TOTAL'.
        <fs_fieldcat>-scrtext_s = 'Custo Total'.
        <fs_fieldcat>-scrtext_m = 'Custo Total'.
        <fs_fieldcat>-scrtext_l = 'Custo Total'.
        <fs_fieldcat>-reptext = 'Custo Total'.
      WHEN 'GASTO'.
        <fs_fieldcat>-scrtext_s = '%Gasto'.
        <fs_fieldcat>-scrtext_m = '%Gasto'.
        <fs_fieldcat>-scrtext_l = '%Gasto'.
        <fs_fieldcat>-reptext = '%Gasto'.
      WHEN 'DATA_FINAL'.
        <fs_fieldcat>-scrtext_s = 'Data Final'.
        <fs_fieldcat>-scrtext_m = 'Data Final'.
        <fs_fieldcat>-scrtext_l = 'Data Final'.
        <fs_fieldcat>-reptext = 'Data Final'.
      WHEN 'DATA_INICIAL'.
        <fs_fieldcat>-scrtext_s = 'Data Inicial'.
        <fs_fieldcat>-scrtext_m = 'Data Inicial'.
        <fs_fieldcat>-scrtext_l = 'Data Inicial'.
        <fs_fieldcat>-reptext = 'Data Inicial'.
      WHEN 'PREPARACAO'.
        <fs_fieldcat>-scrtext_s = 'Preparação'.
        <fs_fieldcat>-scrtext_m = 'Preparação'.
        <fs_fieldcat>-scrtext_l = 'Preparação'.
        <fs_fieldcat>-reptext = 'Preparação'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'MONTAGEM'.
        <fs_fieldcat>-scrtext_s = 'Montagem'.
        <fs_fieldcat>-scrtext_m = 'Montagem'.
        <fs_fieldcat>-scrtext_l = 'Montagem'.
        <fs_fieldcat>-reptext = 'Montagem'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'DESMONTAGEM'.
        <fs_fieldcat>-scrtext_s = 'Desmontagem'.
        <fs_fieldcat>-scrtext_m = 'Desmontagem'.
        <fs_fieldcat>-scrtext_l = 'Desmontagem'.
        <fs_fieldcat>-reptext   = 'Desmontagem'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'PEDIDO_PECAS'.
        <fs_fieldcat>-scrtext_s = 'Pedido Peças'.
        <fs_fieldcat>-scrtext_m = 'Pedido Peças'.
        <fs_fieldcat>-scrtext_l = 'Pedido Peças'.
        <fs_fieldcat>-reptext = 'Pedido Peças'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'MONTAGEM_MEC_GERAL'.
        <fs_fieldcat>-scrtext_s = 'Montagem e mecânica geral'.
        <fs_fieldcat>-scrtext_m = 'Montagem e mecânica geral'.
        <fs_fieldcat>-scrtext_l = 'Montagem e mecânica geral'.
        <fs_fieldcat>-reptext = 'Montagem e mecânica geral'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'ELETRICA'.
        <fs_fieldcat>-scrtext_s = 'Elétrica'.
        <fs_fieldcat>-scrtext_m = 'Elétrica'.
        <fs_fieldcat>-scrtext_l = 'Elétrica'.
        <fs_fieldcat>-reptext = 'Elétrica'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'PNEUS_RODAS'.
        <fs_fieldcat>-scrtext_s = 'Pneus e Rodas'.
        <fs_fieldcat>-scrtext_m = 'Pneus e Rodas'.
        <fs_fieldcat>-scrtext_l = 'Pneus e Rodas'.
        <fs_fieldcat>-reptext = 'Pneus e Rodas'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'LUBRIFICACAO'.
        <fs_fieldcat>-scrtext_s = 'Lubrificação'.
        <fs_fieldcat>-scrtext_m = 'Lubrificação'.
        <fs_fieldcat>-scrtext_l = 'Lubrificação'.
        <fs_fieldcat>-reptext = 'Lubrificação'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'TESTES_FINAIS'.
        <fs_fieldcat>-scrtext_s = 'Testes Finais'.
        <fs_fieldcat>-scrtext_m = 'Testes Finais'.
        <fs_fieldcat>-scrtext_l = 'Testes Finais'.
        <fs_fieldcat>-reptext = 'Testes Finais'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'REFORMA_GERAL'.
        <fs_fieldcat>-scrtext_s = 'Reforma Geral'.
        <fs_fieldcat>-scrtext_m = 'Reforma Geral'.
        <fs_fieldcat>-scrtext_l = 'Reforma Geral'.
        <fs_fieldcat>-reptext = 'Reforma Geral'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'CALDERARIA'.
        <fs_fieldcat>-scrtext_s = 'Calderaria'.
        <fs_fieldcat>-scrtext_m = 'Calderaria'.
        <fs_fieldcat>-scrtext_l = 'Calderaria'.
        <fs_fieldcat>-reptext = 'Calderaria'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'FREIOS'.
        <fs_fieldcat>-scrtext_s = 'Freios'.
        <fs_fieldcat>-scrtext_m = 'Freios'.
        <fs_fieldcat>-scrtext_l = 'Freios'.
        <fs_fieldcat>-reptext = 'Freios'.
        <fs_fieldcat>-edit = abap_true.
      WHEN 'REFORMA'.
        <fs_fieldcat>-scrtext_s = 'Reforma'.
        <fs_fieldcat>-scrtext_m = 'Reforma'.
        <fs_fieldcat>-scrtext_l = 'Reforma'.
        <fs_fieldcat>-reptext = 'Reforma'.
        <fs_fieldcat>-edit = abap_true.
      WHEN OTHERS.
    ENDCASE.

    CASE lv_classe(1).
      WHEN '1' OR '2'.

        IF <fs_fieldcat>-fieldname EQ 'REFORMA_GERAL' OR
           <fs_fieldcat>-fieldname EQ 'MONTAGEM' OR
           <fs_fieldcat>-fieldname EQ 'CALDERARIA' OR
           <fs_fieldcat>-fieldname EQ 'FREIOS' OR
           <fs_fieldcat>-fieldname EQ 'RODAS_PNEUS' OR
           <fs_fieldcat>-fieldname EQ 'REFORMA'.

          DELETE lt_fieldcat WHERE fieldname = <fs_fieldcat>-fieldname.
          CONTINUE.
*          <fs_fieldcat>-no_out = abap_true.

        ENDIF.

      WHEN '3'.

        IF <fs_fieldcat>-fieldname  EQ 'PREPARACAO' OR
          <fs_fieldcat>-fieldname  EQ 'PNEUS_RODAS' OR
          <fs_fieldcat>-fieldname  EQ 'REFORMA_GERAL' OR
          <fs_fieldcat>-fieldname  EQ 'MONTAGEM' OR
          <fs_fieldcat>-fieldname  EQ 'FREIOS' OR
          <fs_fieldcat>-fieldname  EQ 'REFORMA' OR
          <fs_fieldcat>-fieldname  EQ 'CALDERARIA'.

*          <fs_fieldcat>-no_out = abap_true.
          DELETE lt_fieldcat WHERE fieldname = <fs_fieldcat>-fieldname.
          CONTINUE.
        ENDIF.

      WHEN '4'.

        IF <fs_fieldcat>-fieldname EQ 'PREPARACAO' OR
          <fs_fieldcat>-fieldname  EQ 'PEDIDO_PECAS' OR
          <fs_fieldcat>-fieldname  EQ 'MONTAGEM_MEC_GERAL' OR
          <fs_fieldcat>-fieldname  EQ 'ELETRICA' OR
          <fs_fieldcat>-fieldname  EQ 'LUBRIFICACAO' OR
          <fs_fieldcat>-fieldname  EQ 'CALDERARIA' OR
          <fs_fieldcat>-fieldname  EQ 'FREIOS' OR
          <fs_fieldcat>-fieldname  EQ 'PNEUS_RODAS' OR
          <fs_fieldcat>-fieldname  EQ 'MONTAGEM' OR
          <fs_fieldcat>-fieldname  EQ 'REFORMA'.

*          <fs_fieldcat>-no_out = abap_true.
          DELETE lt_fieldcat WHERE fieldname = <fs_fieldcat>-fieldname.
          CONTINUE.
        ENDIF.

      WHEN '5'.

        IF <fs_fieldcat>-fieldname  EQ 'PEDIDO_PECAS' OR
           <fs_fieldcat>-fieldname  EQ 'MONTAGEM_MEC_GERAL' OR
           <fs_fieldcat>-fieldname  EQ 'ELETRICA' OR
           <fs_fieldcat>-fieldname  EQ 'LUBRIFICACAO' OR
           <fs_fieldcat>-fieldname  EQ 'PNEUS_RODAS' OR
           <fs_fieldcat>-fieldname  EQ 'CALDERARIA' OR
           <fs_fieldcat>-fieldname  EQ 'FREIOS' OR
           <fs_fieldcat>-fieldname  EQ 'REFORMA' OR
           <fs_fieldcat>-fieldname  EQ 'REFORMA_GERAL'.

*          <fs_fieldcat>-no_out = abap_true.
          DELETE lt_fieldcat WHERE fieldname = <fs_fieldcat>-fieldname.
          CONTINUE.
        ENDIF.

      WHEN '6'.

        IF <fs_fieldcat>-fieldname  EQ 'PEDIDO_PECAS' OR
           <fs_fieldcat>-fieldname  EQ 'MONTAGEM_MEC_GERAL' OR
           <fs_fieldcat>-fieldname  EQ 'MONTAGEM' OR
           <fs_fieldcat>-fieldname  EQ 'LUBRIFICACAO' OR
           <fs_fieldcat>-fieldname  EQ 'DESMONTAGEM' OR
           <fs_fieldcat>-fieldname  EQ 'REFORMA' OR
           <fs_fieldcat>-fieldname  EQ 'REFORMA_GERAL'.

*          <fs_fieldcat>-no_out = abap_true.
          DELETE lt_fieldcat WHERE fieldname = <fs_fieldcat>-fieldname.
          CONTINUE.
        ENDIF.

      WHEN '7'.

        IF <fs_fieldcat>-fieldname EQ 'PEDIDO_PECAS' OR
          <fs_fieldcat>-fieldname  EQ 'MONTAGEM_MEC_GERAL' OR
          <fs_fieldcat>-fieldname  EQ 'FREIOS' OR
          <fs_fieldcat>-fieldname  EQ 'LUBRIFICACAO' OR
          <fs_fieldcat>-fieldname  EQ 'PNEUS_RODAS' OR
          <fs_fieldcat>-fieldname  EQ 'CALDERARIA' OR
          <fs_fieldcat>-fieldname  EQ 'ELETRICA' OR
          <fs_fieldcat>-fieldname  EQ 'REFORMA' OR
          <fs_fieldcat>-fieldname  EQ 'REFORMA_GERAL'.

*          <fs_fieldcat>-no_out = abap_true.
          DELETE lt_fieldcat WHERE fieldname = <fs_fieldcat>-fieldname.
          CONTINUE.
        ENDIF.

      WHEN '8'.

        IF <fs_fieldcat>-fieldname EQ 'PEDIDO_PECAS' OR
          <fs_fieldcat>-fieldname  EQ 'MONTAGEM_MEC_GERAL' OR
          <fs_fieldcat>-fieldname  EQ 'FREIOS' OR
          <fs_fieldcat>-fieldname  EQ 'LUBRIFICACAO' OR
          <fs_fieldcat>-fieldname  EQ 'PNEUS_RODAS' OR
          <fs_fieldcat>-fieldname  EQ 'CALDERARIA' OR
          <fs_fieldcat>-fieldname  EQ 'ELETRICA' OR
          <fs_fieldcat>-fieldname  EQ 'REFORMA_GERAL'.

*          <fs_fieldcat>-no_out = abap_true.
          DELETE lt_fieldcat WHERE fieldname = <fs_fieldcat>-fieldname.
          CONTINUE.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

  CREATE OBJECT gr_alv
    EXPORTING
      i_parent = cl_gui_docking_container=>screen0.

  CLEAR lw_f4.
  lw_f4-fieldname  = 'STATUS'.
  lw_f4-register   = 'X'.
  APPEND lw_f4 TO lt_f4.

  CREATE OBJECT lo_event_obj.
  SET HANDLER lo_event_obj->on_f4 FOR gr_alv.

  CALL METHOD gr_alv->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4.

  ls_dropdown-handle = '1'.
  ls_dropdown-value = 'Oficina'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '1'.
  ls_dropdown-value = 'Externo'.
  APPEND ls_dropdown TO lt_dropdown.

  CALL METHOD gr_alv->set_drop_down_table
    EXPORTING
      it_drop_down = lt_dropdown.

*  ls_layout-cwidth_opt = abap_true.

  CALL METHOD gr_alv->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = lt_fieldcat
      it_outtab       = gt_saida.

  CALL METHOD gr_alv->register_edit_event
    EXPORTING
      I_event_id = cl_gui_alv_grid=>mc_evt_modified.

  SET HANDLER lo_event_obj->handle_data_changed FOR gr_alv.

  CALL METHOD gr_alv->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDFORM.

FORM on_f4  USING f_fieldname    TYPE lvc_fname
                  f_fieldvalue   TYPE lvc_value
                  fw_row_no      TYPE lvc_s_roid
                  fcl_event_data TYPE REF TO cl_alv_event_data
                  ft_bad_cells   TYPE lvc_t_modi
                  f_display      TYPE char01.

  DATA: lw_alv  TYPE ty_saida,
        lw_modi TYPE lvc_s_modi.

*  DATA: l_origtp TYPE zgnre_origtp.

  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
  CHECK sy-subrc = 0.

  READ TABLE gt_saida INTO lw_alv INDEX fw_row_no-row_id.
  CHECK sy-subrc = 0.

  CASE f_fieldname.
    WHEN 'STATUS'.
      IF lw_alv-status IS INITIAL.

        DATA: lt_rettab TYPE TABLE OF ddshretval.

        DATA: lw_rettab TYPE ddshretval.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
*           ddic_structure  = 'QPCT'
            retfield        = 'KURZTEXT'
            value_org       = 'S'
          TABLES
            value_tab       = gt_qpct
            return_tab      = lt_rettab
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        CHECK sy-subrc = 0.

        READ TABLE lt_rettab INTO lw_rettab INDEX 1.

        CHECK sy-subrc = 0.

        f_fieldvalue = lw_rettab-fieldval.

        CLEAR lw_modi.
        lw_modi-row_id    = fw_row_no-row_id.
        lw_modi-fieldname = f_fieldname.
        lw_modi-value = f_fieldvalue.
        APPEND lw_modi TO <lfst_modi>.

      ENDIF.

      fcl_event_data->m_event_handled = 'X'.

  ENDCASE.

ENDFORM.                                                    " ON_F4
