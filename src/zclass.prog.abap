*&---------------------------------------------------------------------*
*&  Include           ZCLASS
*&---------------------------------------------------------------------*

*DATA: ZPM_GERAR_TOKEN TYPE REF TO ZCL_CARGA_RECEBIMENTO.
*NEW ZPM_GERAR_TOKEN TYPE REF TO ZCL_GERAR_TOKEN_ACESSO.
*CREATE OBJECT ZPM_GERAR_TOKEN.


DATA: ob_web_service TYPE REF TO zcl_webservice.
CREATE OBJECT ob_web_service.



CLASS z_dados DEFINITION.

  PUBLIC SECTION.

    DATA:
      l_numeroamostra    TYPE ztpm_r_amost-numeroamostra,
      l_frota            TYPE ztpm_r_amost-frota,
      l_compartimento    TYPE ztpm_r_amost-compartimento,
      l_responsavel      TYPE ztpm_r_amost-responsavel,
      l_avaliacao        TYPE ztpm_r_amost-avaliacao,
      l_datafinalizacao  TYPE ztpm_r_amost-datafinalizacao,
      l_cliente          TYPE ztpm_r_amost-cliente,
      l_obra             TYPE ztpm_r_amost-obra,
      l_tipocoleta       TYPE ztpm_r_amost-tipocoleta,
      l_statusamostra    TYPE ztpm_r_amost-statusamostra,
      l_laudo            TYPE char8,
      l_hr_km            TYPE ztpm_r_amost-hr_km,
      l_eqktx            TYPE eqkt-eqktx,
      l_iwerk            TYPE equz-iwerk,
      l_eqart            TYPE t370k_t-eqart,
      l_eartx            TYPE t370k_t-eartx,
      l_erdat            TYPE ztpm_r_amost-erdat,
      l_horasoleo        TYPE ztpm_r_amost-horasoleo,
      l_fabricanteoleo   TYPE ztpm_r_amost-fabricanteoleo,
      l_viscosidade      TYPE ztpm_r_amost-viscosidade,
      l_oleotrocado      TYPE ztpm_r_amost-oleotrocado,
      l_volumeadicionado TYPE ztpm_r_amost-volumeadicionado,
      l_datacoleta       TYPE ztpm_r_amost-datacoleta.

    TYPES: BEGIN OF ty_t370k_t,
             eqart TYPE eqart,
             eartx TYPE eartx,
           END OF ty_t370k_t.



    DATA: ls_ztpm_amost           TYPE ztpm_amost,
          lw_ztpm_amost           TYPE TABLE OF ztpm_amost,
          lt_ztpm_l_amost         TYPE TABLE OF ztpm_amost,
          lt_ztpm_amost           TYPE TABLE OF ztpm_amost,
          lt_t370k_t              TYPE TABLE OF ty_t370k_t,
          lw_t370k_t              TYPE ty_t370k_t,
          lw_ztpm_amost_equi      TYPE ztpm_amost_equi,
          lt_ztpm_amost_equi      TYPE TABLE OF ztpm_amost_equi,
          lt_ztpm_amost_equi_graf TYPE TABLE OF ztpm_amost_equi,
          lt_ztpm_amost_graf      TYPE TABLE OF ztpm_amost_graf,
          lt_ztpm_amost_sint      TYPE TABLE OF ztpm_amost_sint.

    DATA:wa_color TYPE          lvc_s_scol,  " Cor para célula
         it_color TYPE TABLE OF lvc_s_scol.  " Cor para célula




*&------------------------------------------------------------------------------*
* Method: Selecionar dados da analise de óleo (Amostra)                         *
*&------------------------------------------------------------------------------*
    METHODS: z_sel_dados_amostra.

*&------------------------------------------------------------------------------*
* Method: Selecionar dados da analise de óleo (Amostra)                         *
*&------------------------------------------------------------------------------*
    METHODS: z_sel_dados_amostra_sint.



*&------------------------------------------------------------------------------*
*   Method: Selecionar status da analise de óleo (Amostra)                       *
*&------------------------------------------------------------------------------*
    METHODS: z_sel_dados_res_amostra
      IMPORTING
        i_numeroamostra    TYPE ztpm_r_amost-numeroamostra
        i_frota            TYPE ztpm_r_amost-frota
        i_compartimento    TYPE ztpm_r_amost-compartimento
        i_avaliacao        TYPE ztpm_r_amost-avaliacao
        i_datafinalizacao  TYPE ztpm_r_amost-datafinalizacao
        i_cliente          TYPE ztpm_r_amost-cliente
        i_obra             TYPE ztpm_r_amost-obra
        i_tipocoleta       TYPE ztpm_r_amost-tipocoleta
        i_statusamostra    TYPE ztpm_r_amost-statusamostra
        i_laudo            TYPE char8
        i_erdat            TYPE ztpm_r_amost-erdat
        i_hr_km            TYPE ztpm_r_amost-hr_km
        i_horasoleo        TYPE ztpm_r_amost-horasoleo
        i_fabricanteoleo   TYPE ztpm_r_amost-fabricanteoleo
        i_viscosidade      TYPE ztpm_r_amost-viscosidade
        i_oleotrocado      TYPE ztpm_r_amost-oleotrocado
        i_volumeadicionado TYPE ztpm_r_amost-volumeadicionado
        i_datacoleta       TYPE ztpm_r_amost-datacoleta
      EXPORTING
        e_situacao         TYPE ztpm_l_amost-situacao
        e_numeroamostra    TYPE ztpm_l_amost-numeroamostra.

*&------------------------------------------------------------------------------*
*   Method: Selecionar Dados da frota                                           *
*&------------------------------------------------------------------------------*
    METHODS: z_des_equipamento
      IMPORTING
        i_eqktx TYPE eqkt-eqktx
        i_iwerk TYPE equz-iwerk
        i_eqart TYPE t370k_t-eqart
        i_eartx TYPE t370k_t-eartx
      EXPORTING
        e_equnr TYPE ztpm_l_amost-frota.


*&------------------------------------------------------------------------------*
*   Method: Selecionar Dados de cadastro amostra.                                        *
*&------------------------------------------------------------------------------*
    METHODS:z_sel_dados_cadastro_amostra.

ENDCLASS.

DATA: obj_dados TYPE REF TO z_dados.
CREATE OBJECT obj_dados.

CLASS z_dados IMPLEMENTATION.


  METHOD: z_sel_dados_cadastro_amostra.

    "Seleciona as informações da amostra cadastrada no site s360.
    SELECT * FROM zpmt0053
    INTO CORRESPONDING FIELDS OF TABLE t_zpmt0053
      WHERE  numeroamostra IN p_amost
      AND  codigoexterno   IN p_equnr
      AND  datacoleta IN p_date.



    CALL SCREEN 0100.



  ENDMETHOD.



*&------------------------------------------------------------------------------*
* Method: Selecionar dados da analise de óleo (Amostra)                         *
*&------------------------------------------------------------------------------*
  METHOD: z_sel_dados_amostra.
    DATA: w_ztpm_r_amost TYPE ztpm_r_amost.

    IF p_agu IS INITIAL AND p_fin IS INITIAL.
      MESSAGE text-014 TYPE 'I' DISPLAY LIKE 'E'.


    ELSE.
      IF p_agu IS NOT INITIAL.
        p_sta-option = 'EQ'.
        p_sta-sign   = 'I'.
        p_sta-low    = 'AGUARDANDO'.
        APPEND p_sta TO p_stat.

        p_sta-option = 'EQ'.
        p_sta-sign   = 'I'.
        p_sta-low    = 'COLETADA'.
        APPEND p_sta TO p_stat.
      ENDIF.

      IF p_fin IS NOT INITIAL.
        p_sta-option = 'EQ'.
        p_sta-sign   = 'I'.
        p_sta-low    = 'FINALIZADA'.
        APPEND p_sta TO p_stat.
      ENDIF.

      IF p_equnr IS NOT INITIAL.
        LOOP AT p_equnr ASSIGNING FIELD-SYMBOL(<w_p_equnr>).
          <w_p_equnr>-low = | { <w_p_equnr>-low ALPHA = OUT }|.
          CONDENSE <w_p_equnr>-low.
        ENDLOOP.
      ENDIF.

      IF p_class IS NOT INITIAL.
        SELECT *
        FROM ztpm_l_amost
        INTO CORRESPONDING FIELDS OF TABLE lt_ztpm_l_amost
          WHERE  numeroamostra   IN p_amost
            AND  frota           IN p_equnr
            AND  id              EQ p_class
            AND  situacao        IN p_stat
            AND  datafinalizacao IN p_data.
*          AND  B~COMPARTIMENTO   EQ P_COMPT.

      ELSE.
        SELECT *
        FROM ztpm_l_amost
        INTO CORRESPONDING FIELDS OF TABLE lt_ztpm_l_amost
          WHERE  numeroamostra   IN p_amost
            AND  frota           IN p_equnr
            AND  situacao        IN p_stat
            AND  datafinalizacao IN p_data.

      ENDIF.


      IF lt_ztpm_l_amost IS NOT INITIAL.
        LOOP AT lt_ztpm_l_amost INTO DATA(ls_ztpm_l_amost).
*Coletando dados do equipamento.
          CLEAR: l_iwerk, l_eqktx, l_eartx, l_eqart.
          z_des_equipamento( EXPORTING
                           i_eqktx = l_eqktx
                           i_iwerk = l_iwerk
                           i_eqart = l_eqart
                           i_eartx = l_eartx
                             IMPORTING
                           e_equnr = ls_ztpm_l_amost-frota ).


          CLEAR: l_laudo, l_avaliacao, w_ztpm_r_amost.
          IF ls_ztpm_l_amost-situacao EQ 'FINALIZADA'.

            SELECT SINGLE *
            FROM ztpm_r_amost
            INTO CORRESPONDING FIELDS OF w_ztpm_r_amost
              WHERE numeroamostra EQ ls_ztpm_l_amost-numeroamostra.

            l_laudo            = '@FA@'.
            l_avaliacao        = '@0J@'.


            CLEAR: wa_color, it_color.
            CASE w_ztpm_r_amost-statusamostra.
              WHEN 'NORMAL'.
                CLEAR wa_color.
                MOVE 'STATUSAMOSTRA'    TO wa_color-fname.
                MOVE '5'                TO wa_color-color-col.
                MOVE '1'                TO wa_color-color-int.
                MOVE '1'                TO wa_color-color-inv.
                APPEND wa_color TO it_color.

              WHEN 'ANORMAL'.
                CLEAR wa_color.
                MOVE 'STATUSAMOSTRA'    TO wa_color-fname.
                MOVE '7'                TO wa_color-color-col.
                MOVE '1'                TO wa_color-color-int.
                MOVE '1'                TO wa_color-color-inv.
                APPEND wa_color TO it_color.
              WHEN 'CRITICO'.

                CLEAR wa_color.
                MOVE 'STATUSAMOSTRA'    TO wa_color-fname.
                MOVE '6'                TO wa_color-color-col.
                MOVE '1'                TO wa_color-color-int.
                MOVE '1'                TO wa_color-color-inv.
                APPEND wa_color TO it_color.

              WHEN 'ATENCAO'.
                CLEAR wa_color.
                MOVE 'STATUSAMOSTRA'    TO wa_color-fname.
                MOVE '3'                TO wa_color-color-col.
                MOVE '1'                TO wa_color-color-int.
                MOVE '1'                TO wa_color-color-inv.
                APPEND wa_color TO it_color.
              WHEN OTHERS.
            ENDCASE.
          ENDIF.

*          Consultar dados da amostras.
          SELECT SINGLE *
          FROM zpmt0023
          INTO @DATA(_zpmt0023)
            WHERE numeroamostra EQ @ls_ztpm_l_amost-numeroamostra.

* Inserindo informações na tabela transparente LW_ZTPM_AMOST para ALV.
          APPEND VALUE #( status_proc       = SWITCH #( _zpmt0023-status_proc WHEN abap_true THEN '@08@' ELSE '@0A@' )
                          frota             = ls_ztpm_l_amost-frota
                          aufnr             = _zpmt0023-aufnr
                          numeroamostra     = ls_ztpm_l_amost-numeroamostra
                          situacao          = ls_ztpm_l_amost-situacao
                          datafinalizacao   = ls_ztpm_l_amost-datafinalizacao
                          nome              = ls_ztpm_l_amost-nome
                          modelo            = ls_ztpm_l_amost-modelo
                          chassiserie       = ls_ztpm_l_amost-chassiserie
                          empresa           = ls_ztpm_l_amost-empresa
                          erdat             = ls_ztpm_l_amost-erdat
                          compartimento     = ls_ztpm_l_amost-compartimento
                          cliente           = ls_ztpm_l_amost-cliente
                          obra              = ls_ztpm_l_amost-obra
                          l_avaliacao        = l_avaliacao
                          statusamostra     = w_ztpm_r_amost-statusamostra
                          hr_km             = w_ztpm_r_amost-hr_km
                          horasoleo         = w_ztpm_r_amost-horasoleo
                          fabricanteoleo    = w_ztpm_r_amost-fabricanteoleo
                          viscosidade       = w_ztpm_r_amost-viscosidade
                          oleotrocado       = w_ztpm_r_amost-oleotrocado
                          volumeadicionado  = w_ztpm_r_amost-volumeadicionado
                          datacoleta        = w_ztpm_r_amost-datacoleta
                          iwerk             = l_iwerk
                          eqktx             = l_eqktx
                          eqart             = l_eqart
                          eartx             = l_eartx
                          cell_color        = it_color
                          avaliacao         = w_ztpm_r_amost-avaliacao
                          l_laudo           = l_laudo ) TO lw_ztpm_amost.


        ENDLOOP.

        CALL SCREEN 0100.

      ELSE.
        MESSAGE text-002 TYPE 'I' DISPLAY LIKE 'E'.

      ENDIF.
    ENDIF.
  ENDMETHOD.


*&------------------------------------------------------------------------------*
*   Method: Selecionar status da analise de óleo (Amostra)                      *
*&------------------------------------------------------------------------------*
  METHOD: z_sel_dados_res_amostra.
    CLEAR: l_laudo, l_statusamostra, l_avaliacao, l_numeroamostra, l_frota,
    l_compartimento, l_datafinalizacao, l_cliente, l_obra, l_tipocoleta, l_statusamostra,
    l_erdat, l_hr_km, l_horasoleo, l_fabricanteoleo, l_viscosidade, l_oleotrocado, l_volumeadicionado, l_datacoleta.
    FREE it_color.

    SELECT SINGLE *
    FROM ztpm_r_amost
    INTO @DATA(_ztpm_l_amost)
      WHERE numeroamostra EQ @e_numeroamostra.

    IF _ztpm_l_amost IS NOT INITIAL.


      l_numeroamostra    = _ztpm_l_amost-numeroamostra    .
      l_frota            = _ztpm_l_amost-frota            .
      l_compartimento    = _ztpm_l_amost-compartimento    .
      l_datafinalizacao  = _ztpm_l_amost-datafinalizacao  .
      l_cliente          = _ztpm_l_amost-cliente          .
      l_obra             = _ztpm_l_amost-obra             .
      l_tipocoleta       = _ztpm_l_amost-tipocoleta       .
      l_statusamostra    = _ztpm_l_amost-statusamostra    .
      l_erdat            = _ztpm_l_amost-erdat            .
      l_hr_km            = _ztpm_l_amost-hr_km            .
      l_horasoleo        = _ztpm_l_amost-horasoleo        .
      l_fabricanteoleo   = _ztpm_l_amost-fabricanteoleo   .
      l_viscosidade      = _ztpm_l_amost-viscosidade      .
      l_oleotrocado      = _ztpm_l_amost-oleotrocado      .
      l_volumeadicionado = _ztpm_l_amost-volumeadicionado .
      l_datacoleta       = _ztpm_l_amost-datacoleta       .


      IF e_situacao EQ 'FINALIZADA'.
        l_laudo            = '@FA@'.
        l_avaliacao        = '@0J@'.      .
      ENDIF.

      CASE l_statusamostra.
        WHEN 'NORMAL'.
          CLEAR wa_color.
          MOVE 'STATUSAMOSTRA'    TO wa_color-fname.
          MOVE '5'                TO wa_color-color-col.
          MOVE '1'                TO wa_color-color-int.
          MOVE '1'                TO wa_color-color-inv.
          APPEND wa_color TO it_color.

        WHEN 'ANORMAL'.
          CLEAR wa_color.
          MOVE 'STATUSAMOSTRA'    TO wa_color-fname.
          MOVE '3'                TO wa_color-color-col.
          MOVE '1'                TO wa_color-color-int.
          MOVE '1'                TO wa_color-color-inv.
          APPEND wa_color TO it_color.
        WHEN 'CRITICO'.

          CLEAR wa_color.
          MOVE 'STATUSAMOSTRA'    TO wa_color-fname.
          MOVE '6'                TO wa_color-color-col.
          MOVE '1'                TO wa_color-color-int.
          MOVE '1'                TO wa_color-color-inv.
          APPEND wa_color TO it_color.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
*&------------------------------------------------------------------------------*
*   Method: Selecionar Dados da frota                                           *
*&------------------------------------------------------------------------------*
  METHOD: z_des_equipamento.
    DATA: _equi TYPE t370k_t.
    e_equnr = |{ e_equnr ALPHA = IN }|.
    SELECT SINGLE *
    FROM eqkt
    INTO @DATA(_eqkt)
      WHERE equnr EQ @e_equnr.


    l_eqktx = _eqkt-eqktx.

    SELECT SINGLE *
     FROM equz
     INTO @DATA(_equz)
       WHERE equnr EQ @e_equnr
         AND datbi EQ '99991231'.

    l_iwerk = _equz-iwerk.

    SELECT SINGLE *
    FROM t370k_t AS a
    INNER JOIN equi AS b ON b~eqart EQ a~eqart
    INTO CORRESPONDING FIELDS OF _equi
      WHERE equnr EQ e_equnr
        AND spras EQ 'PT'.

    l_eqart = _equi-eqart.
    l_eartx = _equi-eartx.

    e_equnr = |{ e_equnr ALPHA = OUT }|.
  ENDMETHOD.

  METHOD: z_sel_dados_amostra_sint.
    DATA: tot_critico TYPE char10,
          tot_anormal TYPE char10,
          tot_normal  TYPE char10,
          tot_atencao TYPE char10,
          tot_geral   TYPE char10.

    IF p_class IS NOT INITIAL.
      SELECT *
      FROM ztpm_r_amost AS a
      INNER JOIN ztpm_l_amost AS b ON b~numeroamostra EQ a~numeroamostra
      INTO CORRESPONDING FIELDS OF TABLE lw_ztpm_amost
        WHERE  a~numeroamostra   IN p_amost
          AND  a~frota           IN p_equnr
          AND  b~id              EQ p_class
          AND  a~datafinalizacao IN p_data.
*          AND  B~COMPARTIMENTO   EQ P_COMPT.

    ELSE.
      SELECT *
      FROM ztpm_r_amost AS a
      INNER JOIN ztpm_l_amost AS b ON b~numeroamostra EQ a~numeroamostra
      INTO CORRESPONDING FIELDS OF TABLE lw_ztpm_amost
        WHERE  a~numeroamostra   IN p_amost
          AND  a~frota           IN p_equnr
          AND  a~datafinalizacao IN p_data.
    ENDIF.

*Preenchendo a tabela da ALV_01.
    IF lw_ztpm_amost IS NOT INITIAL.
      CLEAR: tot_critico, tot_anormal, tot_normal, tot_atencao, tot_geral.
      FREE lt_ztpm_amost_sint.

      LOOP AT lw_ztpm_amost ASSIGNING FIELD-SYMBOL(<lt_lw_ztpm_amost>).

        CLEAR: l_iwerk, l_eqktx, l_eartx, l_eqart.
        z_des_equipamento( EXPORTING
                         i_eqktx = l_eqktx
                         i_iwerk = l_iwerk
                         i_eqart = l_eqart
                         i_eartx = l_eartx
                           IMPORTING
                         e_equnr = <lt_lw_ztpm_amost>-frota ).

        <lt_lw_ztpm_amost>-eqart = l_eqart.
        <lt_lw_ztpm_amost>-eartx = l_eartx.

        CASE <lt_lw_ztpm_amost>-statusamostra.
          WHEN 'CRITICO'.
            ADD 1 TO tot_critico.

          WHEN 'ANORMAL'.
            ADD 1 TO tot_anormal.

          WHEN 'NORMAL'.
            ADD 1 TO tot_normal.

          WHEN 'ATENCAO'.
            ADD 1 TO tot_atencao.
        ENDCASE.
      ENDLOOP.

      tot_geral = ( tot_critico + tot_anormal + tot_normal + tot_atencao ).

      DATA(perc_critico) = ( tot_critico / tot_geral ) * 100.
      DATA(perc_anormal) = ( tot_anormal / tot_geral ) * 100.
      DATA(perc_normal)  = ( tot_normal / tot_geral )  * 100.
      DATA(perc_atencao) = ( tot_atencao / tot_geral ) * 100.

      DATA(critico) = tot_critico && space && '(' && perc_critico && '%' && ')'.
      DATA(anormal) = tot_anormal && space && '(' && perc_anormal && '%' && ')'.
      DATA(normal)  = tot_normal  && space && '(' && perc_normal  && '%' && ')'.
      DATA(atencao) = tot_atencao && space && '(' && perc_atencao && '%' && ')'.

      APPEND VALUE #( total    = tot_geral
                      critico  = critico
                      anormal  = anormal
                      atencao  = atencao
                      normal   = normal ) TO lt_ztpm_amost_sint.

      APPEND VALUE #( total    = tot_geral
                      critico  = tot_critico
                      anormal  = tot_anormal
                      atencao  = tot_atencao
                      normal   = tot_normal ) TO lt_ztpm_amost_graf.




*Preenchendo a tabela da ALV_02.
      DATA(lt_ztpm_amost)  = lw_ztpm_amost.
      LOOP AT lt_ztpm_amost INTO DATA(_lw_ztpm_amost).
        lw_t370k_t-eqart = _lw_ztpm_amost-eqart.
        lw_t370k_t-eartx = _lw_ztpm_amost-eartx.
        APPEND lw_t370k_t TO lt_t370k_t.
      ENDLOOP.
*
      SORT lt_t370k_t BY eqart.
      DELETE ADJACENT DUPLICATES FROM lt_t370k_t COMPARING eqart.


      CLEAR: tot_geral, tot_critico,tot_anormal, tot_atencao, tot_normal.
      CLEAR: perc_critico, perc_anormal, perc_normal, perc_atencao.
      LOOP AT  lt_t370k_t INTO lw_t370k_t.
        LOOP AT lw_ztpm_amost INTO DATA(lt_lw_ztpm_amost) WHERE eqart EQ lw_t370k_t-eqart.
          IF lt_lw_ztpm_amost-eqart EQ lw_t370k_t-eqart.
            CASE lt_lw_ztpm_amost-statusamostra.
              WHEN 'CRITICO'.
                ADD 1 TO tot_critico.
              WHEN 'ANORMAL'.
                ADD 1 TO tot_anormal.

              WHEN 'NORMAL'.
                ADD 1 TO tot_normal.

              WHEN 'ATENCAO'.
                ADD 1 TO tot_atencao.
            ENDCASE.
          ENDIF.
        ENDLOOP.

        tot_geral = ( tot_critico + tot_anormal + tot_normal + tot_atencao ).

        perc_critico = ( tot_critico / tot_geral ) * 100.
        perc_anormal = ( tot_anormal / tot_geral ) * 100.
        perc_normal  = ( tot_normal  / tot_geral )  * 100.
        perc_atencao = ( tot_atencao / tot_geral ) * 100.

        DATA(ob_critico) = perc_critico && '%' .
        DATA(ob_anormal) = perc_anormal && '%' .
        DATA(ob_normal)  = perc_normal  && '%' .
        DATA(ob_atencao) = perc_atencao && '%' .

        IF tot_critico = ' '.
          ob_critico = ' '.
        ENDIF.

        IF tot_anormal = ' '.
          ob_anormal = ' '.
        ENDIF.

        IF tot_normal = ' '.
          ob_normal = ' '.
        ENDIF.

        IF tot_atencao = ' '.
          ob_atencao = ' '.
        ENDIF.

        APPEND VALUE #(   eartx      = lt_lw_ztpm_amost-eartx
                          critico    = tot_critico
                          anormal    = tot_anormal
                          normal     = tot_normal
                          atencao    = tot_atencao
                          p_critico  = ob_critico
                          p_anormal  = ob_anormal
                          p_normal   = ob_normal
                          p_atencao  = ob_atencao
                              total  = tot_geral             ) TO lt_ztpm_amost_equi.


        APPEND VALUE #(   eartx    = lt_lw_ztpm_amost-eartx
                          critico  = tot_critico
                          anormal  = tot_anormal
                          atencao  = tot_atencao
                          normal   = tot_normal
                              ) TO lt_ztpm_amost_equi_graf.

        CLEAR: tot_geral, tot_critico,tot_anormal, tot_atencao, tot_normal.
        CLEAR: perc_critico, perc_anormal, perc_normal, perc_atencao.
      ENDLOOP.

      CALL SCREEN 0200.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.


    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*DATA: OBJ_DADOS TYPE REF TO Z_DADOS.
*CREATE OBJECT OBJ_DADOS.

CLASS lcl_event_handler IMPLEMENTATION.



*&-------------------------------------------------------------------*
*&  CLASS LCL_EVENT_HANDLER IMPLEMENTATION                           *
*&  AUTOR: Anderson Oenning                                                *
*&                                                        *
*&-------------------------------------------------------------------*
  METHOD on_double_click.

    DATA: lc_binario TYPE xstring,
          lt_pdf     TYPE TABLE OF char80.

    DATA wl_ztpm_r_amost  TYPE ztpm_r_amost.
    CLEAR wl_ztpm_r_amost.

    CHECK e_row-rowtype IS INITIAL.

    FREE clicks.
    ADD 1 TO clicks.

    TRY .
        DATA(wa_saida) = obj_dados->lw_ztpm_amost[ e_row ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    FREE obj_dados->lt_ztpm_amost.
    obj_dados->lt_ztpm_amost = obj_dados->lw_ztpm_amost.

    CASE  e_column.

      WHEN 'L_LAUDO'.

        READ TABLE obj_dados->lt_ztpm_amost INTO DATA(w_disponivel) WITH KEY numeroamostra = wa_saida-numeroamostra.
        IF sy-subrc = 0.

          SELECT SINGLE *
        FROM ztpm_r_amost
        INTO wl_ztpm_r_amost
          WHERE numeroamostra EQ w_disponivel-numeroamostra.

          CHECK wl_ztpm_r_amost IS NOT INITIAL.

          CALL FUNCTION 'ZSMARTFORMS_PDF_FILE_PREVIEW'
            EXPORTING
              pdf_data = wl_ztpm_r_amost-laudo.
        ENDIF.

      WHEN 'L_AVALIACAO'.

        READ TABLE obj_dados->lt_ztpm_amost INTO DATA(wl_disponivel) WITH KEY numeroamostra = wa_saida-numeroamostra.
        IF sy-subrc = 0.

          SELECT SINGLE *
         FROM ztpm_r_amost
         INTO wl_ztpm_r_amost
           WHERE numeroamostra EQ wl_disponivel-numeroamostra.

          CHECK wl_ztpm_r_amost IS NOT INITIAL.

          zcl_string=>show_texto( EXPORTING i_string = wl_ztpm_r_amost-avaliacao i_titulo = 'Avaliação Técnica' ).
        ENDIF.

      WHEN 'FROTA'.
        READ TABLE obj_dados->lt_ztpm_amost INTO DATA(ws_disponivel) WITH KEY numeroamostra = wa_saida-numeroamostra.
        IF sy-subrc = 0.
          SET PARAMETER ID 'EQN' FIELD ws_disponivel-frota.
          CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "ON_DOUBLE_CLICK



**********************************************************************
*& Descrição: Criar botões em ALV                                   &*
*& Parâmetro: E_OBJECT->                                            &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD set_toolbar.
    CLEAR wa_toolbar.
    wa_toolbar-function     = 'BTN_GRAF_EQ_MOD'.
    wa_toolbar-icon         =  icon_graphics.
    wa_toolbar-quickinfo    = 'Status das Amostras'.
    wa_toolbar-butn_type    = 0.
    wa_toolbar-text         = 'Amostra por modelo'.
    APPEND wa_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "SET_TOOLBAR


*&******************************************************************&*
*& Descrição: Registra ação nos botões da ALV                       &*
*& Parâmetro: E_UCOMM                                               &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD: get_ucomm.

    CASE e_ucomm.
      WHEN 'BTN_GRAF_EQ_MOD'.
        PERFORM graf.

    ENDCASE.
  ENDMETHOD.                    "GET_UCOMM
ENDCLASS.

CLASS zcl_zpm_gerar_token DEFINITION.
  PUBLIC SECTION.


    TYPES BEGIN OF ty_retorno_token.
    TYPES: access_token TYPE string.
    TYPES: username TYPE string.
    TYPES END OF ty_retorno_token.

    TYPES: BEGIN OF ty_retorno_restado_amostra,
             frota             TYPE string,
             tipocompartimento TYPE string,
             responsavel       TYPE string,
             datarecebimento   TYPE string,
             numerocaixa       TYPE string,
             datafinalizacao   TYPE string,
             cliente           TYPE string,
             nome              TYPE string,
             id                TYPE string,
             notafiscalfrasco  TYPE string,
             tipocoleta        TYPE string,
             statusamostra     TYPE string,
             avaliacao         TYPE string,
           END OF ty_retorno_restado_amostra.

    DATA: lc_json            TYPE string,
*          LC_JSON_AUX        TYPE STRING,
          lc_data            TYPE c LENGTH 10,
          lc_hora            TYPE c LENGTH 08,
          lc_data_hora       TYPE c LENGTH 19,
          lc_numero_rom      TYPE zsdt0001-nr_romaneio,
          lc_inteiro         TYPE i,
          lc_peso            TYPE c LENGTH 09,
          i_name_file        TYPE string,
          lc_retorno         TYPE zde_opus_carga,
          lc_docnum	         TYPE j_1bdocnum,
          lc_nr_romaneio_sai TYPE zde_nr_romaneio_sai,
          e_reason           TYPE string,
          json_retorno       TYPE string,
          json_ret_pdf       TYPE string,
          ls_res_amostra     TYPE ztpm_r_amostra,
          ls_res_lst_amostra TYPE  ztpm_lis_amost,
          ls_lis_amost       TYPE  ztpm_lis_result_t,
          ls_ztpm_l_amost    TYPE  ztpm_l_amost,
          ts_ztpm_l_amost    TYPE TABLE OF ztpm_l_amost,
          ts_ztpm_r_amost    TYPE TABLE OF ztpm_r_amost,
          ls_ztpm_r_amost    TYPE ztpm_r_amost,
          ls_result          TYPE  ztpm_lis_result_t,
          gt_return          TYPE TABLE OF bapiret2,
          ls_gt_return       TYPE bapiret2,
          lc_token           TYPE ty_retorno_token.


    DATA: ls_laudo TYPE ztpm_r_amost-laudo.



*====================================================================
*Method gerar a chave de segurança para acessar as informações s360.
*====================================================================
    METHODS: zgerar_token
      IMPORTING
        token TYPE string.

*=================================================================
*Method consultar os resultados das amostras
*=================================================================
    METHODS: zgerar_res_amostra
      EXPORTING
        token TYPE string.
*      RETURNING
*        VALUE(LS_ZTPM_R_AMOST) TYPE ZTPM_R_AMOST.

*=================================================================
*Method consultar os resultados das amostras
*=================================================================
    METHODS: zgerar_list_amostra
      EXPORTING
        token TYPE string.
*      RETURNING
*        VALUE(LS_RES_LST_AMOSTRA) TYPE ZTPM_LIS_AMOST.

*=================================================================
*Method gerar o laudo técnico em PDF
*=================================================================

    METHODS: zgerar_pdf_result_amostra
      IMPORTING
        numeroamostra TYPE char15
        token         TYPE string
      EXPORTING
        e_laudo       TYPE zde_smartforms_xstring.

ENDCLASS.


CLASS zcl_zpm_gerar_token IMPLEMENTATION.

*====================================================================
*Method gerar a chave de segurança para acessar as informações s360.
*====================================================================
  METHOD: zgerar_token.
    DATA: lt_ls_ztpm_token_s360 TYPE ztpm_token_s360.
    DATA(usuario) = 'intergraco.als@amaggi.com.br'.
    DATA(senha)   = 'amaggi'.

    DATA: tl_texto      TYPE catsxt_longtext_itab.
    DATA: tl_token      TYPE string.

*    Verificar se existe chave valida para usuario.
    SELECT SINGLE *
    FROM ztpm_token_s360
      INTO lt_ls_ztpm_token_s360
      WHERE username EQ usuario.

    IF lt_ls_ztpm_token_s360-token IS INITIAL.

*      Costruindo login de acesso, para gerar a chave de segurança.
      CLEAR: lc_json.
      lc_json = '{"username":' && '"' && usuario &&'",' && '"password":' && '"' && senha && '"}'.

*   Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
      TRY .
          ob_web_service->set_servico( i_servico = 'LS' ).
        CATCH zcx_webservice INTO DATA(lc_exception).
      ENDTRY.
      ob_web_service->set_tipo( i_tipo = 'L' ).

      TRY .
          DATA(var_http) = ob_web_service->url( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
          DATA(lc_uri) = ob_web_service->get_uri(  ).
        CATCH zcx_webservice INTO lc_exception.
      ENDTRY.

      ob_web_service->zif_webservice~abrir_conexao( i_http = var_http ).
      ob_web_service->zif_webservice~consultar(
          EXPORTING
            i_http                     = var_http
            i_xml                      = lc_json
          IMPORTING
*          E_CODE                     = E_CODE
            e_reason                   = e_reason
          RECEIVING
            e_resultado                = json_retorno
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5
        ).


      /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = lc_token ).

      IF sy-subrc IS INITIAL.
        ls_ztpm_token_s360 = VALUE #(
                            mandt    = sy-mandt
                            username = usuario
                            password = senha
                            token    = lc_token-access_token
                            laeda    = sy-datum ).

        MODIFY ztpm_token_s360 FROM ls_ztpm_token_s360.
        COMMIT WORK.
      ELSE.

        RAISE EXCEPTION TYPE zcx_carga
          EXPORTING
            textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgty  = 'E'
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4.
      ENDIF.
    ELSE.

      IF lt_ls_ztpm_token_s360-laeda NE sy-datum.

*    Costruindo login de acesso, para gerar a chave de segurança.
        CLEAR: lc_json.
        lc_json = '{"username":' && '"' && usuario &&'",' && '"password":' && '"' && senha && '"}'.

*   Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
        TRY .
            ob_web_service->set_servico( i_servico = 'LS' ).
          CATCH zcx_webservice INTO lc_exception.
        ENDTRY.
        ob_web_service->set_tipo( i_tipo = 'L' ).

        TRY .
            var_http = ob_web_service->url( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
            lc_uri = ob_web_service->get_uri(  ).
          CATCH zcx_webservice INTO lc_exception.
        ENDTRY.

        ob_web_service->zif_webservice~abrir_conexao( i_http = var_http ).
        ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = var_http
              i_xml                      = lc_json
            IMPORTING
*          E_CODE                     = E_CODE
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).

*      TL_TEXTO      TYPE CATSXT_LONGTEXT_ITAB.
*      TL_TOKEN      TYPE STRING.

        /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = lc_token ).

        IF sy-subrc IS INITIAL.
          DELETE FROM ztpm_token_s360 WHERE username EQ usuario.
          COMMIT WORK.
          ls_ztpm_token_s360 = VALUE #(
                              mandt    = sy-mandt
                              username = usuario
                              password = senha
                              token    = lc_token-access_token
                              laeda    = sy-datum ).

          MODIFY ztpm_token_s360 FROM ls_ztpm_token_s360.
          COMMIT WORK.
        ELSE.

          RAISE EXCEPTION TYPE zcx_carga
            EXPORTING
              textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgty  = 'E'
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        ENDIF.
      ELSE.

        CLEAR lc_token-access_token.
        lc_token-access_token = lt_ls_ztpm_token_s360-token.
      ENDIF.
    ENDIF.
  ENDMETHOD.

*=================================================================
*Method consultar os resultados das amostras
*=================================================================
  METHOD: zgerar_res_amostra.

    LOOP AT ts_ztpm_l_amost INTO DATA(lw_ztpm_l_amost).

*      Verifica se ja existe amostra na tabela ZTPM_R_AMOST.
      SELECT SINGLE * FROM ztpm_r_amost INTO @DATA(_ztpm_r_amost)
      WHERE numeroamostra EQ @lw_ztpm_l_amost-numeroamostra.

      IF _ztpm_r_amost-statusamostra IS INITIAL.

        DATA(url_get) = 'https://s360.com.br' && '/api/v3/resultadoAmostra/view' && '?numeroAmostra=' && lw_ztpm_l_amost-numeroamostra && '&token_type=Bearer&access_token=' && lc_token-access_token.

        DATA(var_http_get) = ob_web_service->url( EXPORTING i_url = CONV #( url_get ) ).

        ob_web_service->zif_webservice~abrir_conexao( i_http = var_http_get ).

        "Documentação: http://www.w3schools.com/tags/ref_httpmethods.asp
        var_http_get->request->set_header_field( EXPORTING name  = '~request_method' value = 'GET' ).

        CLEAR json_retorno.
        ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = var_http_get
            IMPORTING
*          E_CODE                     = E_CODE
                e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).

        IF sy-subrc = 0.
          CLEAR ls_res_amostra.
          /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = ls_res_amostra ).
*        ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = JSON_RETORNO I_TITULO = 'Json Retorno' ).
*        ZTPM_R_A_DADOS_COLET
          IF ls_res_amostra IS NOT INITIAL.

*          Gerar laudo técnico e converter em string para gravar no banco.
            CLEAR ls_laudo.
            zgerar_pdf_result_amostra( EXPORTING
                                       numeroamostra = lw_ztpm_l_amost-numeroamostra
                                       token         = lc_token-access_token
                                       IMPORTING
                                        e_laudo        = ls_laudo ).


            ls_ztpm_r_amost = VALUE #(  mandt            = sy-mandt
                                        numeroamostra    = ls_res_amostra-numeroamostra
                                        frota            = ls_res_amostra-dadoscoletaequipamento-equipamento-frota
                                        chassiserie      = ls_res_amostra-dadoscoletaequipamento-equipamento-chassiserie
                                        id_comp          = ls_res_amostra-dadoscoletaequipamento-compartimento-id
                                        compartimento    = ls_res_amostra-dadoscoletaequipamento-compartimento-nome
                                        responsavel      = ls_res_amostra-responsavel
                                        datarecebimento  = ls_res_amostra-datarecebimento
                                        avaliacao        = ls_res_amostra-avaliacao
                                        datafinalizacao  = ls_res_amostra-datafinalizacao
                                        cliente          = ls_res_amostra-cliente-nome
                                        obra             = ls_res_amostra-obra-nome
                                        tipocoleta       = ls_res_amostra-tipocoleta
                                        statusamostra    = ls_res_amostra-statusamostra
                                        laudo            = ls_laudo
                                        hr_km            = ls_res_amostra-dadoscoletaequipamento-equipamento-horasequipamentocoleta
                                        horasoleo        = ls_res_amostra-dadoscoletageral-horasoleo
                                        fabricanteoleo   = ls_res_amostra-dadoscoletageral-oleo-fabricanteoleo-nome
                                        viscosidade      = ls_res_amostra-dadoscoletageral-oleo-viscosidade-nome
                                        oleotrocado      = ls_res_amostra-dadoscoletageral-oleotrocado
                                        volumeadicionado = ls_res_amostra-dadoscoletageral-volumeadicionado
                                        datacoleta       = ls_res_amostra-dadoscoletageral-datacoleta
                                        erdat           = sy-datum ).

            IF ls_ztpm_r_amost IS NOT INITIAL.
              APPEND ls_ztpm_r_amost TO ts_ztpm_r_amost.
              MODIFY ztpm_r_amost FROM ls_ztpm_r_amost.
              COMMIT WORK.
*            ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

*=================================================================
*Method listar todas as amostras
*=================================================================

  METHOD: zgerar_list_amostra.
*    Method para listar todas as amostras realizadas.

    DATA: tl_texto      TYPE catsxt_longtext_itab.
    DATA: tl_token      TYPE string.
    DATA(empresa) = 'AMAGGI -'.
    DATA(url_get) = 'https://s360.com.br' && '/api/v1/amostra/list' && '?token_type=Bearer&access_token=' && lc_token-access_token.
    DATA(var_http_get) = ob_web_service->url( EXPORTING i_url = CONV #( url_get ) ).

*   Passando boby de acesso.
    lc_json = '{ "nomeObra":' && '"' && empresa && '" }'.

    ob_web_service->zif_webservice~abrir_conexao( i_http = var_http_get ).
    CLEAR json_retorno.
    ob_web_service->zif_webservice~consultar(
        EXPORTING
          i_http                     = var_http_get
          i_xml                      = lc_json
        IMPORTING
*          E_CODE                     = E_CODE
          e_reason                   = e_reason
        RECEIVING
          e_resultado                = json_retorno
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5
      ).

    IF sy-subrc IS INITIAL.
      CLEAR ls_res_lst_amostra.
      /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = ls_res_lst_amostra ).
*      ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = JSON_RETORNO I_TITULO = 'Json Retorno' ).


      IF ls_res_lst_amostra IS NOT INITIAL.

        ts_ztpm_l_amost = VALUE #( FOR wl_ztpm_lis_result IN ls_res_lst_amostra-resultados
                        ( mandt             = sy-mandt
                          numeroamostra     = wl_ztpm_lis_result-numeroamostra
                          codigoexterno     = wl_ztpm_lis_result-codigoexterno
                          situacao          = wl_ztpm_lis_result-situacao
                          datafinalizacao   = |{ wl_ztpm_lis_result-datafinalizacao(4) }{ wl_ztpm_lis_result-datafinalizacao+5(2) }{  wl_ztpm_lis_result-datafinalizacao+8(2) }|
                          nome              = wl_ztpm_lis_result-compartimento-nome
                          modelo            = wl_ztpm_lis_result-equipamento-modelo
                          chassiserie       = wl_ztpm_lis_result-equipamento-chassiserie
                          cliente           = wl_ztpm_lis_result-cliente-nome
                          id                = wl_ztpm_lis_result-cliente-id
                          frota             = wl_ztpm_lis_result-equipamento-frota
                          empresa           = wl_ztpm_lis_result-obra-nome
                          erdat             = sy-datum
                          laeda             = sy-datum ) ).
      ENDIF.

      IF ts_ztpm_l_amost IS NOT INITIAL.
        LOOP AT ts_ztpm_l_amost INTO DATA(lw_ztpm_l_amost).

          SELECT SINGLE *
            FROM ztpm_l_amost
            INTO @DATA(_ztpm_l_amost)
            WHERE numeroamostra EQ @lw_ztpm_l_amost-numeroamostra
              AND situacao      EQ @lw_ztpm_l_amost-situacao.

          IF sy-subrc IS NOT INITIAL.
            MODIFY ztpm_l_amost FROM lw_ztpm_l_amost.
            COMMIT WORK.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.

      RAISE EXCEPTION TYPE zcx_carga
        EXPORTING
          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.


    ENDIF.
  ENDMETHOD.


*=====================================================================
* https://s360.com.br/api//v1/resultadoAmostra/viewPdf?numeroAmostra=
* Method gerar o resultado em formato PDF
*=====================================================================

  METHOD: zgerar_pdf_result_amostra.

    DATA: lr_conv      TYPE REF TO cl_abap_conv_in_ce,
          lr_xstring   TYPE xstring,
          lv_url       TYPE char255,
          lr_string    TYPE string,
          bin_filesize TYPE i,
          it_otf       TYPE TABLE OF ssfcrescl.

    DATA: pdf_table TYPE  rcl_bag_tline,
          pdf_fsize TYPE  i.

    DATA(url_get) = 'https://s360.com.br' && '/api//v1/resultadoAmostra/viewPdf' && '?numeroAmostra=' && numeroamostra && '&token_type=Bearer&access_token=' && token.

    DATA(var_http_get) = ob_web_service->url( EXPORTING i_url = CONV #( url_get ) ).

    ob_web_service->zif_webservice~abrir_conexao( i_http = var_http_get ).

    "Documentação: http://www.w3schools.com/tags/ref_httpmethods.asp
    var_http_get->request->set_header_field( EXPORTING name  = '~request_method' value = 'GET' ).

    CLEAR: json_ret_pdf, e_laudo.
    ob_web_service->zif_webservice~consultar(
        EXPORTING
          i_http                     = var_http_get
        IMPORTING
*          E_CODE                     = E_CODE
            e_reason                 = e_reason
            e_data                   = e_laudo
        RECEIVING
          e_resultado                = json_ret_pdf
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5
      ).
  ENDMETHOD.
ENDCLASS.
