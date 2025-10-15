
*&---------------------------------------------------------------------*
*& CLASS EVENTOS DEFINIÇÃO / ALV
CLASS event DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column sender.
*      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID.

*    METHODS:
*      HANDLE_DOUBLE_CLICK_ORDEM FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN SENDER.
ENDCLASS.




*&---------------------------------------------------------------------*
*& CLASS EVENTOS IMPLEMETAÇÃO / ALV                                           *

DATA(obj_even) = NEW event( ).

CLASS event IMPLEMENTATION.

  METHOD handle_double_click.
    CHECK e_row-rowtype IS INITIAL.
*    PERFORM sel_dados_equip USING e_row e_column-fieldname.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& CLASS ZUTEIS  DEFINITION                                             *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*
CLASS zuteis DEFINITION.
  PUBLIC SECTION.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_HIERARCHY                                      *
*&---------------------------------------------------------------------*
    METHODS: z_checar_equi_hierarchy EXPORTING
                                       e_return TYPE char1.

*-US 158036-26-11-2024-#158036-RJF-Início
*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_HIERARCHY                                      *
*&---------------------------------------------------------------------*

    DATA: gv_stop.
    METHODS: z_bloqueio_custos_frotas.
*-US 158036-26-11-2024-#158036-RJF-Fim

*&---------------------------------------------------------------------*
*& METHOD Z_EQUI_HIERARCHY_READ                                        *
*&---------------------------------------------------------------------*

    METHODS: z_equi_hierarchy_read IMPORTING
                                     equipment TYPE equnr.

*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_STATUS_BAPIS                                      *
*&---------------------------------------------------------------------*
    METHODS: z_atualiza_status_bapis IMPORTING
                                       txt_status TYPE itex132.

*&---------------------------------------------------------------------*
*& METHOD Z_CREATE_COSTCENTER                                          *
*&---------------------------------------------------------------------*
    METHODS: z_create_costcenter IMPORTING
                                   swerk1 TYPE iwerk
                                   swerk2 TYPE iwerk
                                   center TYPE kostl.

    METHODS: z_create_costcenter_dev IMPORTING
                                       swerk1 TYPE iwerk
                                       swerk2 TYPE iwerk
                                       center TYPE kostl.

*&---------------------------------------------------------------------*
*& METHOD Z_DELETE_ZEROS                                               *
*&---------------------------------------------------------------------*
    METHODS: z_delete_zeros CHANGING
                              field TYPE equnr.

*&---------------------------------------------------------------------*
*& METHOD Z_INSERT_DADOS_EMPRESTIMO                                    *
*&---------------------------------------------------------------------*
    METHODS: z_insert_dados_emprestimo IMPORTING
                                         equnr                TYPE equnr
                                         swerk                TYPE swerk
                                         iwerk                TYPE iwerk
                                         qt_dias              TYPE numc3
                                         erdat                TYPE sy-datum
                                         uname                TYPE sy-uname
                                         eqktx                TYPE ktx01
                                         numero_nota          TYPE qmnum
                                         ordem_abast          TYPE daufn
                                         ordem_remon          TYPE ilom_ordst
                                         cent_origem          TYPE swerk
                                         local_origem         TYPE tplnr
                                         nota_zpmt5           TYPE qmnum
                                         imobilizado          TYPE anln1
                                         subimobilizado       TYPE anln2
                                         devolucao_automatica TYPE c.

*&---------------------------------------------------------------------*
*& METHOD Z_DELETE_DADOS_EMPRESTIMO                                    *
*&---------------------------------------------------------------------*
    METHODS: z_delete_dados_emprestimo IMPORTING
                                         equipment TYPE equnr.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_DT_HR_DEVOLUCAO                                    *
*&---------------------------------------------------------------------*
    METHODS: z_checar_dt_hr_devolucao EXPORTING
                                        return TYPE char1
                                      CHANGING
                                        it_tab TYPE ANY TABLE
                                        wa_tab TYPE any.

    METHODS: z_limpar_tela.


    METHODS: z_seleciona_local_tranf IMPORTING werks TYPE werks
                                     EXPORTING local TYPE tplnr.

  PRIVATE SECTION.
    DATA: concac_text TYPE char200.

ENDCLASS.                    "ZUTEIS DEFINITION

*&---------------------------------------------------------------------*
*& CLASS ZUTEIS  IMPLEMENTATION                                         *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

CLASS zuteis IMPLEMENTATION.

**********************************************************************
*& Descrição: Checar hierarquia dos equipamentos.                   &*
*& Parâmetro: EQUIPMENT, RETURN                                     &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD: z_checar_equi_hierarchy.

    CLEAR: it_status_equnr, e_return, wa_saida_emprestimo_equi, wa_hierarchy.
    LOOP AT it_saida_emprestimo_equi INTO wa_saida_emprestimo_equi.
      wa_saida_emprestimo_equi-equnr = |{ wa_saida_emprestimo_equi-equnr ALPHA = IN }|.
*     Verificar se existe equipamento superior.
      CLEAR wa_hierarchy.
      SELECT SINGLE *
      FROM equz AS a
      INNER JOIN equi AS b ON b~equnr EQ a~equnr
      INTO CORRESPONDING FIELDS OF wa_hierarchy
      WHERE a~equnr EQ wa_saida_emprestimo_equi-equnr
        AND a~datbi EQ '99991231'.
*        AND B~EQTYP EQ 'V'.

      IF ( wa_hierarchy-hequi IS NOT INITIAL ).
        SELECT SINGLE *
          FROM eqkt
          INTO @DATA(ls_eqkt_)
            WHERE equnr EQ @wa_hierarchy-equnr.

        wa_status_equnr-equnr = |{ wa_hierarchy-equnr ALPHA = OUT }|. "INFERIOR
        wa_status_equnr-eqktx = ls_eqkt_-eqktx.
        wa_status_equnr-hequi = wa_hierarchy-hequi. "SUPERIOR

        CLEAR ls_eqkt_.
        SELECT SINGLE *
       FROM eqkt
       INTO ls_eqkt_
         WHERE equnr EQ wa_hierarchy-hequi.
        wa_status_equnr-eqktx_ = ls_eqkt_-eqktx.
        wa_status_equnr-eqp_sup = abap_true.

*-US 158036-26-11-2024-#158036-RJF-Início
        me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

        APPEND wa_status_equnr TO it_status_equnr.
        CLEAR: wa_status_equnr.", WA_HIERARCHY, WA_SAIDA_EMPRESTIMO_EQUI.
        return_status = abap_true.

*     Verificar se existe equipamento superior.
        FREE it_hierarchy.
        SELECT *
        FROM equz AS a
        INNER JOIN equi AS b ON b~equnr EQ a~equnr
        INTO CORRESPONDING FIELDS OF TABLE it_hierarchy
        WHERE a~hequi EQ wa_saida_emprestimo_equi-equnr
          AND a~datbi EQ '99991231'.

        IF ( it_hierarchy IS NOT INITIAL ).
          CLEAR wa_hierarchy.
          LOOP AT it_hierarchy[] INTO wa_hierarchy WHERE hequi = wa_saida_emprestimo_equi-equnr.
            IF sy-subrc = 0.
              SELECT SINGLE *
              FROM eqkt
              INTO @DATA(ls_eqkt)
                WHERE equnr EQ @wa_hierarchy-equnr.

              wa_status_equnr-equnr = |{ wa_hierarchy-equnr ALPHA = OUT }|. "INFERIOR
              wa_status_equnr-eqktx = ls_eqkt-eqktx.
              wa_status_equnr-hequi = wa_hierarchy-hequi. "SUPERIOR

              CLEAR ls_eqkt.
              SELECT SINGLE *
             FROM eqkt
             INTO ls_eqkt
               WHERE equnr EQ wa_hierarchy-hequi.
              wa_status_equnr-eqktx_ = ls_eqkt-eqktx.
              wa_status_equnr-eqp_inf = abap_true.

*-US 158036-26-11-2024-#158036-RJF-Início
              me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

              APPEND wa_status_equnr TO it_status_equnr.
              CLEAR: wa_status_equnr, wa_saida_emprestimo_equi.
              return_status = abap_true.
            ENDIF.
          ENDLOOP.
        ENDIF.

        FREE it_status_hequi.
        MOVE it_status_equnr TO it_status_hequi.
        LOOP AT it_status_hequi INTO wa_status_equnr.
          IF wa_status_equnr-eqp_sup EQ 'X'.
            wa_status_equnr-hequi = |{ wa_status_equnr-hequi ALPHA = IN }|.
            SELECT *
            FROM equz AS a
            INNER JOIN equi AS b ON b~equnr EQ a~equnr
              INTO CORRESPONDING FIELDS OF TABLE st_equz
              WHERE a~hequi EQ wa_status_equnr-hequi
                AND a~datbi EQ '99991231'
                AND b~eqtyp NE 'V'
                AND b~eqtyp NE 'A' "FF - 05.04.2024 - ins
                AND b~eqtyp NE '1' "FF - 22.11.2023 - ins
                AND b~eqtyp NE '2'
                AND b~eqtyp NE '3'
                AND b~eqtyp NE '4'.

            LOOP AT st_equz INTO w_equz WHERE hequi = wa_status_equnr-hequi.
              IF sy-subrc = 0.
                CLEAR ls_eqkt.
                SELECT SINGLE *
                 FROM eqkt
                 INTO ls_eqkt
                   WHERE equnr EQ w_equz-equnr.

                wa_status_hequi-equnr = |{ w_equz-equnr ALPHA = OUT }|. "INFERIOR
                wa_status_hequi-eqktx = ls_eqkt-eqktx.
                wa_status_hequi-hequi = w_equz-hequi. "SUPERIOR

                CLEAR ls_eqkt.
                SELECT SINGLE *
               FROM eqkt
               INTO ls_eqkt
                 WHERE equnr EQ w_equz-hequi.
                wa_status_hequi-eqktx_ = ls_eqkt-eqktx.
                wa_status_hequi-eqp_inf = abap_true.

*-US 158036-26-11-2024-#158036-RJF-Início
                me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

                APPEND wa_status_hequi TO it_status_equnr.
*                  APPEND WA_STATUS_HEQUI TO IT_STATUS.
                CLEAR: wa_status_hequi, wa_saida_emprestimo_equi, w_equz.

              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
*        ENDIF.
        CONTINUE.
      ENDIF.

      FREE it_hierarchy.
      SELECT *
      FROM equz AS a
      INNER JOIN equi AS b ON b~equnr EQ a~equnr
      INTO CORRESPONDING FIELDS OF TABLE it_hierarchy
      WHERE a~hequi EQ wa_saida_emprestimo_equi-equnr
        AND a~datbi EQ '99991231'.
*        AND B~EQTYP EQ 'V'.

      LOOP AT it_hierarchy[] INTO wa_hierarchy WHERE hequi = wa_saida_emprestimo_equi-equnr.
        IF sy-subrc = 0.
          SELECT SINGLE *
          FROM eqkt
          INTO @DATA(gs_eqkt)
            WHERE equnr EQ @wa_hierarchy-equnr.

          wa_status_equnr-equnr = |{ wa_hierarchy-equnr ALPHA = OUT }|. "INFERIOR
          wa_status_equnr-eqktx = gs_eqkt-eqktx.
          wa_status_equnr-hequi = wa_hierarchy-hequi. "SUPERIOR

          CLEAR ls_eqkt.
          SELECT SINGLE *
         FROM eqkt
         INTO gs_eqkt
           WHERE equnr EQ wa_hierarchy-hequi.
          wa_status_equnr-eqktx_ = gs_eqkt-eqktx.
          wa_status_equnr-eqp_inf = abap_true.
        ENDIF.

*-US 158036-26-11-2024-#158036-RJF-Início
        me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

        APPEND wa_status_equnr TO it_status_equnr.
        CLEAR: wa_status_equnr, wa_hierarchy, wa_saida_emprestimo_equi.
        return_status = abap_true.
      ENDLOOP.

      FREE it_status_hequi.
      MOVE it_status_equnr TO it_status_hequi.
      LOOP AT it_status_hequi INTO wa_status_equnr.
        IF wa_status_equnr-eqp_inf EQ 'X'.
          wa_status_equnr-equnr = |{ wa_status_equnr-equnr ALPHA = IN }|.
          SELECT *
          FROM equz AS a
          INNER JOIN equi AS b ON b~equnr EQ a~equnr
            INTO CORRESPONDING FIELDS OF TABLE st_equz
            WHERE a~hequi EQ wa_status_equnr-equnr
              AND a~datbi EQ '99991231'
              AND b~eqtyp NE 'V'
              AND b~eqtyp NE 'A' "FF - 05.04.2024 - ins
              AND b~eqtyp NE '1' "FF - 22.11.2023 - ins
              AND b~eqtyp NE '2'
              AND b~eqtyp NE '3'
              AND b~eqtyp NE '4'.

          LOOP AT st_equz INTO w_equz WHERE hequi = wa_status_equnr-equnr.
            IF sy-subrc = 0.
              CLEAR ls_eqkt.
              SELECT SINGLE *
               FROM eqkt
               INTO ls_eqkt
                 WHERE equnr EQ w_equz-equnr.

              wa_status_hequi-equnr = |{ w_equz-equnr ALPHA = OUT }|. "INFERIOR
              wa_status_hequi-eqktx = ls_eqkt-eqktx.
              wa_status_hequi-hequi = w_equz-hequi. "SUPERIOR

              CLEAR ls_eqkt.
              SELECT SINGLE *
             FROM eqkt
             INTO ls_eqkt
               WHERE equnr EQ w_equz-hequi.
              wa_status_hequi-eqktx_ = ls_eqkt-eqktx.
              wa_status_hequi-eqp_inf = abap_true.

*-US 158036-26-11-2024-#158036-RJF-Início
              me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

              APPEND wa_status_hequi TO it_status_equnr.
              CLEAR: wa_status_hequi, wa_saida_emprestimo_equi, w_equz.

            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT it_status_equnr BY equnr.
    DELETE ADJACENT DUPLICATES FROM it_status_equnr COMPARING equnr.
    SORT it_status_equnr BY hequi.

*    Verificar se equipamento ja existe na tabela IT_SAIDA_EMPRESTIMO_EQUI.
    LOOP AT it_saida_emprestimo_equi INTO DATA(w_dev).
      LOOP AT it_status_equnr INTO DATA(_status) WHERE hequi = w_dev-equnr.
        IF _status-eqp_inf IS INITIAL.
          DELETE it_status_equnr INDEX sy-tabix.
          CLEAR: _status.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      LOOP AT it_status_equnr INTO DATA(status) WHERE equnr = w_dev-equnr.
        IF status-eqp_sup IS INITIAL.
          DELETE it_status_equnr INDEX sy-tabix.
          CLEAR: status.
          CONTINUE.
        ENDIF.
      ENDLOOP.
      CLEAR: status, _status.
    ENDLOOP.

  ENDMETHOD.                    "Z_CHECAR_EQUI_HIERARCHY
*-US 158036-26-11-2024-#158036-RJF-Início
  METHOD: z_bloqueio_custos_frotas.
    IF wa_saida_emprestimo_equi-equnr IS NOT INITIAL.

      DATA: ln_kostln(4)   TYPE n,
            ln_kostlnx(10) TYPE n,
            ln_kostlm(4)   TYPE n,
            ln_kostlmx(10) TYPE n.

      BREAK rfreitas.

      SELECT equnr, eqtyp, eqart, kostl
        UP TO 1 ROWS
        FROM itob " *ITOB-EQTYP - Objetos técnicos PM (EQUI, local de instalação)
        INTO @DATA(wa_itob)
        WHERE equnr EQ @wa_saida_emprestimo_equi-equnr.
      ENDSELECT.
      IF sy-subrc IS INITIAL.

        IF  wa_itob-eqtyp EQ '1'
            OR wa_itob-eqtyp EQ '2'
            OR wa_itob-eqtyp EQ '3'
            OR wa_itob-eqtyp EQ '4'
            OR wa_itob-eqtyp EQ 'A'.

          ln_kostlmx = wa_itob-kostl.
          ln_kostlm = ln_kostlmx+6(4).

          SELECT * FROM zpmt0001
          INTO TABLE @DATA(it_zpmt0001)
          WHERE eqtyp EQ @wa_itob-eqtyp
            AND eqart EQ @wa_itob-eqart.
*            AND kostlg EQ @ln_kostlm.

          IF sy-subrc IS INITIAL.
            SORT it_zpmt0001 BY eqtyp eqart.
            LOOP AT it_zpmt0001 INTO DATA(wa_pmt0001) WHERE eqtyp = wa_itob-eqtyp
                                                        AND eqart = wa_itob-eqart.
              ln_kostln  = wa_pmt0001-kostlg.
              IF sy-subrc IS NOT INITIAL OR ln_kostln NE ln_kostlm.
*              msg01 = 'Verificar categoria informada x centro de custos tabela ZPM0010.'.
*              MESSAGE  msg01 TYPE 'E'.

                IF it_status_equnr[] IS INITIAL
                 AND ( sy-dynnr EQ '0200' OR sy-dynnr EQ '0400' ).
                  MESSAGE TEXT-e03 TYPE 'I' DISPLAY LIKE 'E'.
                  gv_stop = abap_on.
                  EXIT.
                ELSEIF sy-dynnr EQ '0300'.
                  wa_status_equnr-status = icon_alert.
                  wa_status_hequi-status = icon_alert.
                  wa_status_equnr-det = TEXT-e03.
                  wa_status_hequi-det = TEXT-e03.
                ENDIF.
              ELSE.
                IF sy-dynnr EQ '0300'.
                  wa_status_equnr-status = icon_complete.
                  wa_status_hequi-status = icon_complete.
                ENDIF.
              ENDIF.
            ENDLOOP.

          ELSE.
            IF it_status_equnr[] IS INITIAL
             AND ( sy-dynnr EQ '0200' OR sy-dynnr EQ '0400' ).
              MESSAGE TEXT-e03 TYPE 'I' DISPLAY LIKE 'E'.
              gv_stop = abap_on.
              EXIT.
            ELSEIF sy-dynnr EQ '0300'.
              wa_status_equnr-status = icon_alert.
              wa_status_hequi-status = icon_alert.
              wa_status_equnr-det = TEXT-e03.
              wa_status_hequi-det = TEXT-e03.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
*-US 158036-26-11-2024-#158036-RJF-Fim
**********************************************************************
*& Descrição: Atualizar status das BAPIS no alv da tela 0200        &*
*& Parâmetro: TXT_STATUS                                            &*
*& Atributos: ME->CONCAC_TEXT                                       &*
********************************************************************&*
  METHOD z_atualiza_status_bapis.

* Atualizar status das BAPIS na ALV, conforme os dados passados para o método.
*    INDICE = INDICE + 1.
*
*    CONCATENATE INDICE TXT_STATUS INTO ME->CONCAC_TEXT SEPARATED BY SPACE.
*
*    WA_STATUS_BAPIS-TXT_STATUS = ME->CONCAC_TEXT.
*    APPEND WA_STATUS_BAPIS TO IT_STATUS_BAPIS.
*
*    STATUS_BAPIS = 'X'.
*
*    CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "Z_CHECAR_STATUS_BAPIS

**********************************************************************
*& Descrição: Retornar hierarquia dos equipamentos                  &*
*& Parâmetro: EQUIPMENT                                             &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD z_equi_hierarchy_read.

* Função retorna as hierarquias dos equipamentos.
* INFERIOR || SUPERIOR

    CALL FUNCTION 'EQUI_HIERARCHY_READ'
      EXPORTING
        equipment  = equipment
        level_down = 01
      TABLES
        hier_tab   = it_hierarchy.

    DELETE it_hierarchy[] INDEX 1.
*    DELETE IT_HIERARCHY[] WHERE EQTYP NE 'V'.

  ENDMETHOD.                    "Z_EQUI_HIERARCHY_READ

**********************************************************************
*& Descrição: Deletar 000000's das variáveis                        &*
*& Parâmetro: FIELD                                                 &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD z_delete_zeros.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = field
      IMPORTING
        output = field.

  ENDMETHOD.                    "Z_DELETE_ZEROS_VAR

**********************************************************************
*& Descrição: Montar centro de custo                                &*
*& Parâmetro: IWERK1 IWERK2 CENTER                                  &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD z_create_costcenter.

* Para criar o centro de custo, primeiramente concatenamos '0', em seguida
* os 2 primeiros dígitos do centro destino, '0' novamente, os 2 ultimos
* dígitos do centro destino, e os 4 ultimos dígitos do
* centro origem.

* Exemplo.: Centro atual: 0150210085 -> Centro destino: 0150070085.

    CONCATENATE '0' swerk1(2) '0' swerk2+2(2) center+6(4) INTO at_costcenter_destino.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Construir centro de custo destino'
                  filial_origem   = center
                  filial_destino  = center
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim

  ENDMETHOD.                    "Z_CREATE_COSTCENTER


**********************************************************************
*& Descrição: Montar centro de custo                                &*
*& Parâmetro: IWERK1 IWERK2 CENTER                                  &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD z_create_costcenter_dev.

* Para criar o centro de custo, primeiramente concatenamos '0', em seguida
* os 2 primeiros dígitos do centro destino, '0' novamente, os 2 ultimos
* dígitos do centro destino, e os 4 ultimos dígitos do
* centro origem.

* Exemplo.: Centro atual: 0150210085 -> Centro destino: 0150070085.

    CONCATENATE '0' swerk1(2) '0' swerk2+2(2) center+6(4) INTO at_costcenter_destino.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'monta centro de custo '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim

  ENDMETHOD.                    "Z_CREATE_COSTCENTER

**********************************************************************
*& Descrição: Inserior dados dos eqptos emprestados na tabela z     &*
*& Parâmetro: EQUNR, SWERK, IWERK, QT_DIAS, ERDAT, UNAME, EQKTX     &*
*             NUMERO_NOTA, ORDEM_ABAST, ORDEM_REMON                 &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD z_insert_dados_emprestimo.

* Inserir dados dos equipamentos que estão sendo emprestados na tabela ZEQUI_EMPRESTIMO,
* esse médoto é chamado no final da das BAPIS.

    wa_zequi_emprestimo-equnr       = equnr.
    wa_zequi_emprestimo-swerk       = swerk.
    wa_zequi_emprestimo-iwerk       = iwerk.
    wa_zequi_emprestimo-qt_dias     = qt_dias.
    wa_zequi_emprestimo-erdat       = erdat.
    wa_zequi_emprestimo-uname       = uname.
    wa_zequi_emprestimo-eqktx       = eqktx.
    wa_zequi_emprestimo-notif_no    = numero_nota.
    wa_zequi_emprestimo-standorder  = ordem_abast.
    wa_zequi_emprestimo-settlorder  = ordem_remon.
    wa_zequi_emprestimo-cent_origem = cent_origem.
    wa_zequi_emprestimo-local_origem =  local_origem.
    wa_zequi_emprestimo-notif_zpm5  =  nota_zpmt5.
    wa_zequi_emprestimo-imobilizado = imobilizado.  "FF - 10/04/2024 #137726
    wa_zequi_emprestimo-subimobilizado = subimobilizado.  "FF - 10/04/2024 #137726
    wa_zequi_emprestimo-dt_fim_emprestimo = erdat + qt_dias. " Rubenilson - 23.12.24 - US138088
    wa_zequi_emprestimo-devolucao_automatica = devolucao_automatica." Rubenilson - 23.12.24 - US138088

    MODIFY zequi_emprestimo FROM wa_zequi_emprestimo.
    COMMIT WORK.
    WAIT UP TO 02 SECONDS.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'inserir dados ZEQUI_EMPRESTIMO'
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.

    IF it_zpmt0079 IS NOT INITIAL.
      MODIFY zpmt0079 FROM TABLE it_zpmt0079.
      COMMIT WORK.
    ENDIF.
*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_INSERT_TABLE_EMPRESTIMO

**********************************************************************
*& Descrição: Deletar dados dos eqptos da tabela z                  &*
*  no momento da devolução                                          &*
*& Parâmetro: EQUNR                                                 &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD z_delete_dados_emprestimo.



  ENDMETHOD.                    "Z_DELETE_DADOS_EMPRESTIMO

**********************************************************************
*& Descrição: Checar se parâmetros estão preenchidos para devolver  &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD: z_checar_dt_hr_devolucao.
    CLEAR: return_status, wa_saida_equi_responsavel, wa_zequi_emprestimo,
           it_zequi_emprestimo.

* Verifica se se foi selecionado algum equipamento.

    LOOP AT it_saida_equi_responsavel INTO wa_saida_equi_responsavel
      WHERE cbx_devolver = 'X'.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE i836(sd) WITH TEXT-001.
        return_status = 'X'.
      ELSE.

        SELECT SINGLE *
          FROM zequi_emprestimo
          INTO wa_zequi_emprestimo
         WHERE equnr = wa_saida_equi_responsavel-equnr.

*     Verifica se foi informada uma data para o eqpto selecionado.

        IF wa_saida_equi_responsavel-dt_devolucao IS INITIAL.
          MESSAGE i836(sd) WITH TEXT-036 DISPLAY LIKE 'W'.

          return_status = 'X'.

*     Verifica se a data de devolução é maior que a data de empréstimo.

        ELSEIF wa_saida_equi_responsavel-dt_devolucao < wa_zequi_emprestimo-erdat.
          MESSAGE i836(sd) WITH TEXT-044 TEXT-045 DISPLAY LIKE 'W'.

          return_status = 'X'.

*      Verifica se foi informada uma hora para eqpto selecionado.

        ELSEIF wa_saida_equi_responsavel-hr_devolucao IS INITIAL.
          MESSAGE i836(sd) WITH TEXT-037 DISPLAY LIKE 'W'.

          return_status = 'X'.

        ENDIF.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.                    "Z_CHECAR_DT_HR_DEVOLUCAO

  METHOD z_limpar_tela.

    CLEAR: tbx_equipamento, tbx_centro.

    CALL METHOD obj_alv_0110->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "Z_LIMPAR_TELA

  METHOD z_seleciona_local_tranf.
    DATA: ws_iflo TYPE iflo.
    "Seleciona o local de transferencia.

    SELECT SINGLE tplnr FROM iflo INTO local WHERE iwerk EQ werks AND fltyp EQ 'Y'.

  ENDMETHOD.                    "Z_LIMPAR_TELA
ENDCLASS.                    "ZUTEIS IMPLEMENTATION


*&----------------------------------------------------------------------------*
*& CLASS SELECIONA_DADOS DEFINITION                                           *
*& AUTOR: ENIO JESUS                                                          *
*& 15.07.2015                                                                 *
*&----------------------------------------------------------------------------*
CLASS z_seleciona_dados DEFINITION.

  PUBLIC SECTION.
*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0110                                       *
*&----------------------------------------------------------------------------*
    METHODS: z_seleciona_dados_tela_0110.
*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0120                                       *
*&----------------------------------------------------------------------------*
    METHODS: z_seleciona_dados_tela_0120.
*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0130                                       *
*&----------------------------------------------------------------------------*
    METHODS  z_seleciona_dados_tela_0130.
*&----------------------------------------------------------------------------*
*& METHOD Z_AUTHORITY_CHECK                                                   *
*&----------------------------------------------------------------------------*
    METHODS: z_authority_check  IMPORTING
                                  object TYPE char7
                                  id     TYPE char5
                                  field  TYPE swerk
                                  return TYPE sy-subrc.

*&----------------------------------------------------------------------------*
*& METHOD Z_STATUS_EQUIPAMENTO                                                *
*&----------------------------------------------------------------------------*
    METHODS: get_status_equnr IMPORTING i_equnr  TYPE equnr
                              EXPORTING e_return LIKE abap_false.

    METHODS: z_detalhes_equipamento IMPORTING
                                      i_equnr TYPE equnr.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_EMPRESTADO                                     *
*&---------------------------------------------------------------------*
    METHODS: z_checar_equi_emprestado IMPORTING
                                        msg    TYPE char1
                                        return TYPE char1.


*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_TELA_RESPONSABILIDADE                               *
*&---------------------------------------------------------------------*
    METHODS: z_atualiza_tela_respons.

*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_TELA_POS_EMPRESTIMO                               *
*&---------------------------------------------------------------------*
    METHODS: z_atualiza_tela_emprestimo.

  PRIVATE SECTION.
    DATA: wa_return TYPE bapiret2,
          at_return TYPE sy-subrc.

ENDCLASS.                    "Z_SELECIONA_DADOS DEFINITION

*&---------------------------------------------------------------------*
*& CLASS SELECIONA_DADOS IMPLEMENTATION                                *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

CLASS z_seleciona_dados IMPLEMENTATION.

************************************************************************
*& Descrição: Método de seleção sub-tela 0110                         &*
*& Este é o método principal, pois chama-se a partir daqui os métodos &*
*& de pesquisa por nº de equipamento e centro.                        &*
**********************************************************************&*

  METHOD: z_seleciona_dados_tela_0110.
    DATA: ls_imptt TYPE imptt,
          wa_fleet TYPE fleet.
    CLEAR: it_msg_return, it_equi, it_saida_equi_disponiveis, it_zequi_emprestimo,
           it_msg_return.

*   Busca pelo nº do equipamento.
    IF ( tbx_equipamento IS NOT INITIAL ).

      tbx_equipamento = |{ tbx_equipamento ALPHA = IN }|.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_equi
        FROM equi AS a
        INNER JOIN equz AS b ON b~equnr EQ a~equnr
        WHERE a~equnr EQ tbx_equipamento
         AND  a~eqtyp IN ( 'A', 'V', '1', '2', '3', '4' ) "FF - 22/11/20023 e 05/04/2024 type A - ins
          AND b~iwerk EQ tbx_centro
          AND b~datbi EQ '99991231'.

      DELETE ADJACENT DUPLICATES FROM it_equi.

*   Busca por centro de custo.
    ELSE.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_equi
        FROM equi AS a
        INNER JOIN equz AS b ON b~equnr EQ a~equnr
        WHERE b~iwerk EQ tbx_centro
          AND a~eqtyp IN ( 'A', 'V', '1', '2', '3', '4' ) "FF - 22.11.2023 e 05/04/2024 type A - ins
          AND b~datbi EQ '99991231'.
    ENDIF.

    SORT it_equi BY equnr.
    DELETE ADJACENT DUPLICATES FROM it_equi.

*    Verificar status do equipamento, se esta ativo.
    LOOP AT it_equi ASSIGNING FIELD-SYMBOL(<w_equi>).
      get_status_equnr( EXPORTING i_equnr  = <w_equi>-equnr
                        IMPORTING e_return = return_status ).

      IF ( return_status EQ abap_true ).
        <w_equi>-marc = abap_true.
      ENDIF.
    ENDLOOP.

    DELETE it_equi WHERE marc EQ abap_true.

    SELECT *
    FROM zequi_emprestimo
    INTO TABLE it_zequi_emprestimo
    FOR ALL ENTRIES IN it_equi
    WHERE equnr = it_equi-equnr.

*   Separando equipamentos disponiveis e equipamento emprestado.
    IF it_equi IS NOT INITIAL AND tbx_equipamento IS NOT INITIAL.
      LOOP AT it_zequi_emprestimo INTO wa_zequi_emprestimo WHERE equnr = tbx_equipamento.
        DELETE it_equi WHERE equnr = wa_zequi_emprestimo-equnr.
      ENDLOOP.
    ELSE.
      LOOP AT it_zequi_emprestimo INTO wa_zequi_emprestimo.
        DELETE it_equi WHERE equnr = wa_zequi_emprestimo-equnr.
      ENDLOOP.
    ENDIF.

*   Pegar descrição do equipamento.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_eqkt
      FROM eqkt
   FOR ALL ENTRIES IN it_equi
     WHERE equnr = it_equi-equnr.

    IF tbx_equipamento IS INITIAL.
      LOOP AT it_equi INTO wa_equi.

*     Pegar centro origem do eqpto.
        CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
          EXPORTING
            equipment         = wa_equi-equnr
          IMPORTING
            data_specific_exp = wa_data_specific_exp
            data_general_exp  = wa_data_general.


*      Verificar se o equipamento possue equipamento superior.
        FREE t_equz.
        SELECT SINGLE a~iwerk a~equnr b~objnr b~eqtyp a~datbi a~hequi
        FROM equz AS a
        INNER JOIN equi AS b ON b~equnr = a~equnr
        INTO t_equz
        WHERE a~equnr EQ wa_equi-equnr
          AND a~iwerk EQ tbx_centro
          AND a~datbi EQ '99991231'
          AND b~eqtyp IN ( 'A','V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins
        IF t_equz-hequi IS NOT INITIAL.
          wa_saida_equi_disponiveis-eq_sup = abap_true.
          p_eq_sup = abap_true.
        ENDIF.

*         Verfificando se existe equipamento inferior.
        SELECT SINGLE *
        FROM equz AS a
        INNER JOIN equi AS b ON b~equnr EQ a~equnr
          INTO CORRESPONDING FIELDS OF gt_equz
         WHERE a~hequi EQ wa_equi-equnr
           AND a~iwerk EQ tbx_centro
           AND a~datbi EQ '99991231'
           AND b~eqtyp IN ( 'A','V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins
        IF gt_equz IS NOT INITIAL.
          wa_saida_equi_disponiveis-eq_inf = abap_true.
        ENDIF.
*

        READ TABLE it_eqkt INTO wa_eqkt WITH KEY equnr = wa_equi-equnr.
        IF sy-subrc EQ 0.

*          Verificar se equipamento possue ponto de medição combustivel.
          CLEAR ls_imptt.
          SELECT SINGLE b~atnam
          FROM imptt AS a
          INNER JOIN cabn AS b ON b~atinn EQ a~atinn
          INNER JOIN equi AS c ON c~equnr EQ wa_equi-equnr AND c~objnr EQ a~mpobj
          INTO ls_imptt
            WHERE a~mptyp EQ 'M'
              AND a~inact EQ ' '
              AND b~atnam EQ 'COMBUSTIVEL'
              AND c~eqtyp IN ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins

          IF ls_imptt IS NOT INITIAL.
            wa_saida_equi_disponiveis-cbx_ord_abast = abap_true.

*         Captura os dados do veículo;
            CLEAR wa_fleet.
            SELECT SINGLE *
              INTO CORRESPONDING FIELDS OF wa_fleet
              FROM fleet AS a
             INNER JOIN equi AS b ON b~equnr = wa_equi-equnr
                                 AND a~objnr = b~objnr
              WHERE key_num NE ' ' OR tq_combustivel_1 NE '0'.
            IF wa_fleet IS INITIAL.
              CLEAR wa_msg_return.
              wa_msg_return-msg = |'Equipamento { wa_equi-equnr ALPHA = OUT }não possuí capacidade tq. combustível'|.
              APPEND wa_msg_return TO it_msg_return.
            ENDIF.
          ENDIF.

          wa_saida_equi_disponiveis-eqktx = wa_eqkt-eqktx.
          wa_saida_equi_disponiveis-equnr = |{ wa_equi-equnr ALPHA = OUT }|.
          wa_saida_equi_disponiveis-iwerk = wa_data_general-maintplant.

          APPEND wa_saida_equi_disponiveis TO it_saida_equi_disponiveis.
          CLEAR: wa_saida_equi_disponiveis, wa_equi, wa_eqkt, p_eq_sup.
        ENDIF.
      ENDLOOP.

      DESCRIBE TABLE it_saida_equi_disponiveis LINES g_tabstrip-qtd1.
      CLEAR: wa_saida_equi_disponiveis, wa_eqkt.

    ELSE.
      CLEAR p_eq_sup.
      LOOP AT it_equi INTO wa_equi WHERE equnr = tbx_equipamento.

*      Verfificando se existe equipamento superior.
        SELECT SINGLE a~iwerk a~equnr b~objnr b~eqtyp a~datbi a~hequi
        FROM equz AS a
        INNER JOIN equi AS b ON b~equnr = a~equnr
        INTO t_equz
        WHERE a~equnr EQ wa_equi-equnr
          AND a~iwerk EQ tbx_centro
          AND a~datbi EQ '99991231'
          AND b~eqtyp IN ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins
        IF t_equz-hequi IS NOT INITIAL.
          wa_saida_equi_disponiveis-eq_sup = abap_true.
          p_eq_sup = abap_true.
        ENDIF.

*      Verfificando se existe equipamento inferior.
        SELECT SINGLE *
        FROM equz AS a
        INNER JOIN equi AS b ON b~equnr EQ a~equnr
          INTO @DATA(z_equz)
         WHERE a~hequi EQ @wa_equi-equnr
           AND a~iwerk EQ @tbx_centro
           AND a~datbi EQ '99991231'
           AND b~eqtyp IN ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins
        IF z_equz IS NOT INITIAL.
          wa_saida_equi_disponiveis-eq_inf = abap_true.
        ENDIF.

*     Pegar centro origem do eqpto.
        CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
          EXPORTING
            equipment         = wa_equi-equnr
          IMPORTING
            data_specific_exp = wa_data_specific_exp
            data_general_exp  = wa_data_general.

**     Verifica se o usuário possuí permissão para a pesquisa informada.
*      AUTHORITY-CHECK OBJECT 'I_SWERK' ID 'SWERK'
*      FIELD WA_DATA_GENERAL-MAINTPLANT.
*
*      IF ( SY-SUBRC IS INITIAL ).
        READ TABLE it_eqkt INTO wa_eqkt WITH KEY equnr = wa_equi-equnr.
        IF sy-subrc EQ 0.

*        Verificar se equipamento possue ponto de medição combustivel.
          CLEAR ls_imptt.
          SELECT SINGLE b~atnam
          FROM imptt AS a
          INNER JOIN cabn AS b ON b~atinn EQ a~atinn
          INNER JOIN equi AS c ON c~equnr EQ wa_equi-equnr AND c~objnr EQ a~mpobj
          INTO ls_imptt
            WHERE a~mptyp EQ 'M'
              AND a~inact EQ ' '
              AND b~atnam EQ 'COMBUSTIVEL'
              AND c~eqtyp IN ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023  e 05/04/2024 type A - ins
          IF ls_imptt IS NOT INITIAL.
            wa_saida_equi_disponiveis-cbx_ord_abast = abap_true.

*          Captura os dados do veículo/Se existe capacidade do tanque;
            CLEAR wa_fleet.
            SELECT SINGLE *
              INTO CORRESPONDING FIELDS OF wa_fleet
              FROM fleet AS a
             INNER JOIN equi AS b ON b~equnr = wa_equi-equnr
                                 AND a~objnr = b~objnr
              WHERE key_num NE ' ' OR tq_combustivel_1 NE '0'.
            IF wa_fleet IS INITIAL.
              CLEAR wa_msg_return.
              wa_msg_return-msg = |'Equipamento { wa_equi-equnr ALPHA = OUT }não possuí capacidade tq. combustível'|.
              APPEND wa_msg_return TO it_msg_return.
            ENDIF.
          ENDIF.

          wa_saida_equi_disponiveis-eqktx = wa_eqkt-eqktx.
          wa_saida_equi_disponiveis-equnr = |{ wa_equi-equnr ALPHA = OUT }|.
          wa_saida_equi_disponiveis-iwerk = wa_data_general-maintplant.

          APPEND wa_saida_equi_disponiveis TO it_saida_equi_disponiveis.
          CLEAR: wa_saida_equi_disponiveis, wa_equi, wa_eqkt, t_equz, w_equz, gt_equz.
        ENDIF.
      ENDLOOP.

      DESCRIBE TABLE it_saida_equi_disponiveis LINES g_tabstrip-qtd1.
      CLEAR: wa_saida_equi_disponiveis, wa_eqkt.
    ENDIF.
  ENDMETHOD.                    "Z_BUSCA_EQUIPAMENTO

************************************************************************
*& Descrição: Text-box de pesquisa por nº de eqpto sub-tela 0110      &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD: TBX_BUSCA_EQUIPM_DISPONIVEIS.

*    CHECK RETURN_STATUS IS INITIAL.
*    Z_STATUS_EQUIPAMENTO( EQUIPMENT = TBX_BUSC_EQUIPAMENTO ).
*    Z_DETALHES_EQUIPAMENTO( EQUIPMENT = TBX_BUSC_EQUIPAMENTO ).

*  ENDMETHOD.                    "Z_TBX_BUSCA_POR_EQUIPAMENTO

************************************************************************
*& Descrição: Text-box de pesquisa por nº do centro sub-tela 0110     &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD: TBX_BUSCA_CENTRO_DISPONIVEIS.
*    CLEAR IT_SAIDA_EQUI_DISPONIVEIS.
*
*    SELECT *
*      FROM ZEQUI_EMPRESTIMO
*      INTO TABLE IT_ZEQUI_EMPRESTIMO.
*
*    SELECT *
*      INTO CORRESPONDING FIELDS OF TABLE IT_EQUI
*      FROM EQUI AS A
*     INNER JOIN ITOB AS B ON A~EQUNR = B~EQUNR
*     INNER JOIN JEST AS C ON A~OBJNR = C~OBJNR
*     WHERE A~EQTYP IN ('V','F','U')
*       AND B~DATBI EQ '99991231'
*       AND B~SWERK EQ TBX_BUSC_CENTRO
*       AND C~STAT  NE 'I0076'
*       AND C~STAT  NE 'I0320'
*       AND C~INACT NE 'X'.
*
*    IF ( SY-SUBRC IS NOT INITIAL ).
*      CHECK ITABSTRIP-ACTIVETAB = 'TAB_DISPONIVEL'.
*      MESSAGE S836(SD) WITH TEXT-040 TEXT-041 DISPLAY LIKE 'E'.
*
*    ELSE.
*
*      SORT IT_EQUI.
*      DELETE ADJACENT DUPLICATES FROM IT_EQUI.
*
**   Deletar os equipamentos da tabela de saída que estão emprestados.
*
*      Z_CHECAR_EQUI_EMPRESTADO(    MSG   = ' '
*                                RETURN   = RETURN_STATUS ).
*
*    ENDIF.
*
*  ENDMETHOD.                    "TBX_BUSC_POR_CENTRO

************************************************************************
*& Descrição: Método de seleção sub-tela 0120                         &*
*& Este é o método principal, pois chama-se a partir daqui os métodos &*
*& de pesquisa por equipamento e centro de custo.                     &*
**********************************************************************&*
  METHOD z_seleciona_dados_tela_0120.
    REFRESH: it_saida_equi_emprestados, it_zequi_emprestimo.
    FREE it_zequi_emprestimo.
*   Busca pelo nº do equipamento
    IF ( tbx_equipamento IS NOT INITIAL ).

      SELECT *
        FROM zequi_emprestimo
  INTO TABLE it_zequi_emprestimo
       WHERE equnr EQ tbx_equipamento
         AND cent_origem EQ tbx_centro.
    ELSE.

      SELECT *
        FROM zequi_emprestimo
       INTO TABLE it_zequi_emprestimo
       WHERE cent_origem EQ tbx_centro.
    ENDIF.

*   Emitir mensagem de erro apenas se estiver na aba referênte aos
*   equipamentos emprestados da sub-tela 0120.

*      IF ITABSTRIP-ACTIVETAB = 'TAB_EMPRESTADOS'.
*        MESSAGE S836(SD) WITH TEXT-010 TEXT-034 DISPLAY LIKE 'W'.
*    ENDIF.

**   Busca pelo nº do centro
*    ELSEIF ( S_WERKS IS NOT INITIAL ).
*      TBX_BUSCA_CENTRO_EMPRESTADOS( ).
*    ENDIF.

    LOOP AT it_zequi_emprestimo INTO wa_zequi_emprestimo.

*     Verifica se o usuário possuí permissão para a pesquisa informada.
*      AUTHORITY-CHECK OBJECT 'I_SWERK' ID 'CENT_ORIGEM'
*      FIELD WA_ZEQUI_EMPRESTIMO-CENT_ORIGEM.

      IF ( sy-subrc IS INITIAL ).
        wa_saida_equi_emprestados-equnr       = wa_zequi_emprestimo-equnr.
        wa_saida_equi_emprestados-cent_origem = wa_zequi_emprestimo-cent_origem.
*        WA_SAIDA_EQUI_EMPRESTADOS-SWERK       = WA_ZEQUI_EMPRESTIMO-SWERK.
        wa_saida_equi_emprestados-iwerk       = wa_zequi_emprestimo-iwerk.
        wa_saida_equi_emprestados-qt_dias     = wa_zequi_emprestimo-qt_dias.
        wa_saida_equi_emprestados-uname       = wa_zequi_emprestimo-uname.
        wa_saida_equi_emprestados-erdat       = wa_zequi_emprestimo-erdat.
        wa_saida_equi_emprestados-eqktx       = wa_zequi_emprestimo-eqktx.
        APPEND wa_saida_equi_emprestados TO it_saida_equi_emprestados.

      ELSE.
        MESSAGE s836(sd) WITH TEXT-031 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE it_zequi_emprestimo LINES g_tabstrip-qtd2.
    CLEAR: wa_saida_equi_emprestados, wa_zequi_emprestimo.
  ENDMETHOD.                    "Z_SELECIONA_DADOS_SCREEN_0120

************************************************************************
*& Descrição: Text-box de pesquisa por nº do eqpto sub-tela 0120      &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD: TBX_BUSCA_EQUIPM_EMPRESTADOS.
*
*    SELECT *
*      FROM ZEQUI_EMPRESTIMO
*INTO TABLE IT_ZEQUI_EMPRESTIMO
*     WHERE EQUNR EQ TBX_EQUIPAMENTO.
*
*    CHECK SY-SUBRC IS NOT INITIAL.
*
**   Emitir mensagem de erro apenas se estiver na aba referênte aos
**   equipamentos emprestados da sub-tela 0120.
*
*    IF ITABSTRIP-ACTIVETAB     = 'TAB_EMPRESTADOS'.
*      MESSAGE S836(SD) WITH TEXT-010 TEXT-034 DISPLAY LIKE 'W'.
*
**   Emitir mensagem de erro apenas se estiver na aba referênte aos
**   equipamentos emprestados da sub-tela 0130.
*
*    ELSEIF ITABSTRIP-ACTIVETAB = 'TAB_RESPONSAVEL'.
*      MESSAGE S836(SD) WITH TEXT-010 TEXT-035 DISPLAY LIKE 'W'.
*    ENDIF.
*
*    ITABSTRIP-ACTIVETAB = 'TAB_DISPONIVEL'.

*  ENDMETHOD.                    "Z_TBX_BUSCA_POR_EQUIPAMENTO

************************************************************************
*& Descrição: Text-box de pesquisa por nº do centro sub-tela 0120     &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD: TBX_BUSCA_CENTRO_EMPRESTADOS.

*    SELECT *
*      FROM ZEQUI_EMPRESTIMO AS A
*      INTO TABLE IT_ZEQUI_EMPRESTIMO
*     WHERE SWERK EQ TBX_BUSC_CENTRO.
*
*    CHECK SY-SUBRC IS NOT INITIAL.
*
**   Emitir mensagem de erro apenas se estiver na aba referênte aos
**   equipamentos emprestados da sub-tela 0120.
*
*    IF ITABSTRIP-ACTIVETAB = 'TAB_EMPRESTADOS'.
*      MESSAGE S836(SD) WITH TEXT-042 TEXT-041 DISPLAY LIKE 'E'.
*    ENDIF.

*  ENDMETHOD.                    "TBX_BUSCA_POR_CENTRO_0120

************************************************************************
*& Descrição: Método de seleção sub-tela 0130                         &*
*& Este é o método principal, pois chama-se a partir daqui os métodos &*
*& de pesquisa por equipamento e centro de custo.                     &*
**********************************************************************&*
  METHOD z_seleciona_dados_tela_0130.
    REFRESH: it_saida_equi_responsavel, it_zequi_emprestimo.

    DATA: lt_cellcolor  TYPE lvc_t_scol, "Rubenilson - 23.12.24 - US138088
          wa_cellcolor  TYPE lvc_s_scol, "Rubenilson - 23.12.24 - US138088
          lv_dt_fim_emp TYPE datum. "Rubenilson - 23.12.24 - US138088

*   Busca pelo nº do equipamento.
    IF ( tbx_equipamento IS NOT INITIAL ).

      SELECT *
        FROM zequi_emprestimo
  INTO TABLE it_zequi_emprestimo
       WHERE equnr EQ tbx_equipamento
         AND iwerk EQ tbx_centro.

*   Busca pelo nº do centro.
    ELSE.
      SELECT *
       FROM zequi_emprestimo
       INTO TABLE it_zequi_emprestimo
       WHERE iwerk EQ tbx_centro.

    ENDIF.

    LOOP AT it_zequi_emprestimo INTO wa_zequi_emprestimo.
      IF ( sy-subrc IS INITIAL ).
        wa_saida_equi_responsavel-equnr       = |{ wa_zequi_emprestimo-equnr ALPHA = OUT }|.
*        WA_SAIDA_EQUI_RESPONSAVEL-SWERK       = WA_ZEQUI_EMPRESTIMO-SWERK.
        wa_saida_equi_responsavel-cent_origem = wa_zequi_emprestimo-cent_origem.
        wa_saida_equi_responsavel-iwerk       = wa_zequi_emprestimo-iwerk.
        wa_saida_equi_responsavel-qt_dias     = wa_zequi_emprestimo-qt_dias.
        wa_saida_equi_responsavel-uname       = wa_zequi_emprestimo-uname.
        wa_saida_equi_responsavel-eqktx       = wa_zequi_emprestimo-eqktx.
        wa_saida_equi_responsavel-erdat       = wa_zequi_emprestimo-erdat.
        wa_saida_equi_responsavel-notif_no = wa_zequi_emprestimo-notif_no.
        wa_saida_equi_responsavel-standorder = wa_zequi_emprestimo-standorder.
        wa_saida_equi_responsavel-settlorder = wa_zequi_emprestimo-settlorder.


*** Inicio - Rubenilson - 24.12.24 - US138088
        FREE: lt_cellcolor.

        lv_dt_fim_emp = wa_zequi_emprestimo-erdat + wa_zequi_emprestimo-qt_dias.
        IF lv_dt_fim_emp >= sy-datum.
          wa_cellcolor-fname = 'QT_DIAS'.
          wa_cellcolor-color-col = 5.
          wa_cellcolor-color-int = 1.
          wa_cellcolor-color-inv = 1.

          INSERT wa_cellcolor INTO lt_cellcolor INDEX 1.

        ELSE.

          wa_cellcolor-fname = 'QT_DIAS'.
          wa_cellcolor-color-col = 6.
          wa_cellcolor-color-int = 1.
          wa_cellcolor-color-inv = 1.

          INSERT wa_cellcolor INTO lt_cellcolor INDEX 1.

        ENDIF.

        wa_saida_equi_responsavel-cellcolor = lt_cellcolor.
*** Fim - Rubenilson - 24.12.24 - US138088

        APPEND wa_saida_equi_responsavel TO it_saida_equi_responsavel.

      ELSE.
        MESSAGE s836(sd) WITH TEXT-031 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = tbx_equipamento
      IMPORTING
        output = tbx_equipamento.

    DESCRIBE TABLE it_zequi_emprestimo LINES g_tabstrip-qtd3.
    CLEAR: wa_saida_equi_responsavel, wa_zequi_emprestimo.
  ENDMETHOD.                    "Z_SELECIONA_DADOS_SCREEN_0130

************************************************************************
*& Descrição: Text-box de pesquisa por nº do centro sub-tela 0130     &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD TBX_BUSCA_CENTRO_RESPONSAVEL.
*    SELECT *
*      FROM ZEQUI_EMPRESTIMO
*      INTO TABLE IT_ZEQUI_EMPRESTIMO
*     WHERE IWERK EQ TBX_CENTRO.
*
*    CHECK SY-SUBRC IS NOT INITIAL.
*
*    IF ITABSTRIP-ACTIVETAB = 'TAB_RESPONSAVEL'.
*      MESSAGE S836(SD) WITH TEXT-043 TEXT-041 DISPLAY LIKE 'E'.
*    ENDIF.
*  ENDMETHOD.                    "TBX_BUSCA_POR_CENTRO_0130

************************************************************************
*& Descrição.: Verificar se o usuário possuí permissão ao objeto      &*
*& Atributos.: ME->AT_RETURN                                          &*
*& Parâmetros: OBJECT, ID, FIELD                                      &*
**********************************************************************&*
  METHOD z_authority_check.
    CLEAR return_status.

    AUTHORITY-CHECK OBJECT object
    ID id FIELD field.

*    CHECK SY-SUBRC IS NOT INITIAL.
*    RETURN_STATUS = 'X'.

  ENDMETHOD.                    "Z_AUTHORITY_CHECK

************************************************************************
*& Descrição.: Obter status do equipamento                            &*
*& Atributos.: Globais                                                &*
*& Parâmetros: EQUIPMENT                                              &*
**********************************************************************&*
  METHOD get_status_equnr.
    CLEAR: it_system_status, it_user_status, wa_system_status,
           wa_return, return_status..

    DATA v_equnr TYPE equnr.

    CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
      EXPORTING
        equipment     = i_equnr
        language      = sy-langu
      IMPORTING
        return        = wa_return
      TABLES
        system_status = it_system_status
        user_status   = it_user_status.

    READ TABLE it_system_status INTO wa_system_status INDEX 1.

    IF ( wa_system_status-status = 'I0076' OR
         wa_system_status-status = 'I0320' ).
      e_return = abap_true.
    ENDIF.
  ENDMETHOD.                    "Z_STATUS_EQUIPAMENTO

************************************************************************
*& Descrição.: Verificar se o eqpto está emprestado                   &*
*& Atributos.: Globais                                                &*
*& Parâmetros: MSG, RETURN                                            &*
**********************************************************************&*
  METHOD z_checar_equi_emprestado.
    CLEAR return_status.

    LOOP AT it_zequi_emprestimo INTO wa_zequi_emprestimo.
      READ TABLE it_equi INTO wa_equi WITH KEY equnr = wa_zequi_emprestimo-equnr.

      CHECK sy-subrc IS INITIAL.
      DELETE it_equi WHERE equnr = wa_zequi_emprestimo-equnr.

      IF msg EQ 'X'.
        CHECK itabstrip-activetab = 'TAB_DISPONIVEIS'.
        MESSAGE s836(sd) WITH TEXT-022 wa_zequi_emprestimo-equnr TEXT-032 DISPLAY LIKE 'W'.
        itabstrip-activetab = 'TAB_EMPRESTADOS'.
      ENDIF.

      return_status = 'X'.
    ENDLOOP.
  ENDMETHOD.                    "Z_CHECAR_EQUI_EMPRESTADO

************************************************************************
*& Descrição.: Detalhes do equipamento                                &*
*& Atributos.: WA_DATA_SPECIFIC_EXP, WA_RETURN                        &*
*& Parâmetros:                                                        &*
**********************************************************************&*
  METHOD z_detalhes_equipamento.

    CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
      EXPORTING
        equipment         = i_equnr
      IMPORTING
        data_specific_exp = wa_data_specific_exp
        return            = wa_return.

    IF ( wa_data_specific_exp-equicatgry NE 'V' OR
         wa_data_specific_exp-equicatgry NE 'A' OR "FF - 05.04.2024 - ins
         wa_data_specific_exp-equicatgry NE '1' OR "FF - 22.11.2023 - ins
         wa_data_specific_exp-equicatgry NE '2' OR
         wa_data_specific_exp-equicatgry NE '3' OR
         wa_data_specific_exp-equicatgry NE '4' OR
         wa_data_specific_exp-equicatgry NE 'F' ).

      MESSAGE s836(sd) WITH TEXT-002 TEXT-003 DISPLAY LIKE 'W'.
      return_status = 'X'.
    ENDIF.
  ENDMETHOD.                    "Z_DETALHES_EQUIPAMENTO

************************************************************************
*& Descrição.: Atualizar tela após empréstimo de eqpto                &*
*& Atributos.:                                                        &*
*& Parâmetros:                                                        &*
**********************************************************************&*
  METHOD z_atualiza_tela_emprestimo.

    "137696 Ajustar ZPM0026 para chamar endpoint de equipamento quando fizer uma transferencia - PSA

    LOOP AT it_saida_emprestimo_equi ASSIGNING FIELD-SYMBOL(<send_mobman_emprestimo>).
      IF <send_mobman_emprestimo>-equnr IS NOT INITIAL.
        SUBMIT zpmr0078 WITH s_equnr EQ <send_mobman_emprestimo>-equnr WITH s_iwerk EQ tbx_centro_destino AND RETURN.
      ENDIF.
    ENDLOOP.


*   Fazer a busca dos equipamentos que foram emprestados, e mudar para a
*   aba dos equipamentos emprestados.
    CLEAR: tbx_centro_destino, tbx_qt_dias, it_saida_emprestimo_equi.


    z_seleciona_dados_tela_0110( ).
    z_seleciona_dados_tela_0120( ).

*    FREE GT_EXC_BUTTON.

    wa_stable-col = 'X'.
    wa_stable-row = 'X'.
    CALL METHOD obj_alv_0110->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    tbx_centro          = tbx_centro.
    itabstrip-activetab = g_tabstrip-tab2.


    MESSAGE i836(sd) WITH TEXT-019 DISPLAY LIKE 'S'.

    LEAVE TO SCREEN 0.

  ENDMETHOD.                    "Z_ATUALIZA_TELA_POS_OPERACAO

  METHOD z_atualiza_tela_respons.

    "137696 Ajustar ZPM0026 para chamar endpoint de equipamento quando fizer uma transferencia - PSA

    LOOP AT it_saida_dev_equi ASSIGNING FIELD-SYMBOL(<send_mobman_devolucao>).
      IF <send_mobman_devolucao>-equnr IS NOT INITIAL AND <send_mobman_devolucao>-iwerk IS NOT INITIAL.
        SUBMIT zpmr0078 WITH s_equnr EQ <send_mobman_devolucao>-equnr WITH s_iwerk EQ <send_mobman_devolucao>-cent_origem AND RETURN.
      ENDIF.
    ENDLOOP.

*  FAZER A BUSCA DOS EQUIPAMENTOS QUE FORAM DEVOLVIDOS, E MUDAR PARA A
*  ABA DOS EQUIPAMENTOS DEVOLVIDOS.


    z_seleciona_dados_tela_0130( ).


    wa_stable-col = 'X'.
    wa_stable-row = 'X'.
    CALL METHOD obj_alv_0130->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    tbx_centro          = tbx_centro.
    itabstrip-activetab = g_tabstrip-tab3.
    MESSAGE i836(sd) WITH TEXT-038 TEXT-039 DISPLAY LIKE 'S'.

    CLEAR: p_dev_por, p_data_dev, p_hora_dev, it_saida_dev_equi.
    LEAVE TO SCREEN 0.
  ENDMETHOD.
ENDCLASS.                    "Z_SELECIONA_DADOS IMPLEMENTATION



*&-----------------------------------------------------------------------------*
*& CLASS ZBAPIS DEFINITION                                                     *
*& AUTOR: ENIO JESUS                                                           *
*& 13.07.2015                                                                  *
*&-----------------------------------------------------------------------------*
CLASS zbapis DEFINITION.
  PUBLIC SECTION.

    DATA: zlocal_insta  TYPE tplnr,
          zlocal_origem TYPE tplnr.

*-US 158036-26-11-2024-#158036-RJF-Início
    DATA: gv_stopzb.
*-US 158036-26-11-2024-#158036-RJF-Fim

    TYPES: BEGIN OF ty_selc_plan,
             warpl TYPE vimplastat-warpl,
             mptyp TYPE vimplastat-mptyp,
             strat TYPE vimplastat-strat,
             objnr TYPE vimplastat-objnr,
           END OF ty_selc_plan.
*&---------------------------------------------------------------------*
*& METHOD INICIA PROCESSO DAS BAPIS                                    *
*&---------------------------------------------------------------------*
    METHODS: z_iniciar_processo_emprestimo.
    METHODS: z_iniciar_processo_devolucao.


*&---------------------------------------------------------------------*
*& METHOD LOCALIZAR ID_CENTRO DE TRABALHO                                           *
*&---------------------------------------------------------------------*
    METHODS: z_localizar_id_centro_trabalho IMPORTING
                                                      objty TYPE cr_objty
                                                      werks TYPE werks_d
                                                      arbpl TYPE arbpl
                                            EXPORTING objid TYPE cr_objid.




*&---------------------------------------------------------------------*
*& METHOD NOTA DE EMPRÉSTIMO                                           *
*&---------------------------------------------------------------------*
    METHODS: z_criar_nota_emprestimo IMPORTING
                                       equipment  TYPE equnr
                                       short_text TYPE qmtxt
                                       priority   TYPE priok
                                       code_group TYPE qmgrp
                                       coding     TYPE qmcod
                                       notif_type TYPE char2
                                       maintplant TYPE iwerk
                                       planplant  TYPE iwerk
                                       funct_loc  TYPE tplnr
                                       plangroup  TYPE ingrp
                                       pm_wkctr   TYPE lgwid
                                       tplnr      TYPE tplnr.

    METHODS: z_close_nota_emprestimo IMPORTING
                                       refdate  TYPE sy-datum
                                       reftime  TYPE syuzeit
                                       notif_no TYPE qmnum.


    METHODS: z_modify_nota_emprestimo IMPORTING
                                        equipment TYPE equnr
                                        planplant TYPE iwerk
                                        tplnr     TYPE tplnr
                                        notif_no  TYPE qmnum.



*&---------------------------------------------------------------------*
*& METHOD ORDENS DE MANUTENÇÃO                                         *
*&---------------------------------------------------------------------*
    METHODS: z_criar_ordens_manutenc IMPORTING
                                       order_type   TYPE aufart
                                       short_text   TYPE auftext
                                       planplant    TYPE iwerk
                                       bus_area     TYPE gsber
                                       funct_loc    TYPE tplnr
                                       mn_wk_ctr    TYPE gewrk
                                       plant        TYPE wergw
                                       maintplant   TYPE swerk
                                       loc_bus_area TYPE gsber
                                       plangroup    TYPE ingrp
                                       equipment    TYPE equnr
                                       costcenter   TYPE kostl
                                       pmacttype    TYPE char3
                                       priority     TYPE priok
                                       activity     TYPE vornr
                                       control_key  TYPE steus
                                       description  TYPE ltxa1
                                       create_notif TYPE char01.


    METHODS: z_criar_ordens_manutenc_get
      RETURNING VALUE(numero_ordem) TYPE char12.

    METHODS: z_encerrar_todas_ordens IMPORTING
                                       equipment  TYPE equnr
                                       standorder TYPE daufn
                                       settlorder TYPE ilom_ordst.

    METHODS: z_get_data_ordem IMPORTING
                                aufnr         TYPE aufnr
                              EXPORTING
                                ws_data_ordem TYPE viaufkst.

    METHODS: z_criar_ordens_manutenc_dev IMPORTING
                                           order_type   TYPE aufart
                                           short_text   TYPE auftext
                                           planplant    TYPE iwerk
                                           bus_area     TYPE gsber
                                           funct_loc    TYPE tplnr
                                           mn_wk_ctr    TYPE gewrk
                                           plant        TYPE wergw
                                           maintplant   TYPE swerk
                                           loc_bus_area TYPE gsber
                                           plangroup    TYPE ingrp
                                           equipment    TYPE equnr
                                           costcenter   TYPE kostl
                                           pmacttype    TYPE char3
                                           priority     TYPE priok
                                           activity     TYPE vornr
                                           control_key  TYPE steus
                                           description  TYPE ltxa1
                                           create_notif TYPE char01.

    METHODS: z_criar_ordens_manutenc_get_dv
      RETURNING VALUE(numero_ordem) TYPE char12.

    METHODS: z_encerrar_todas_ordens_dev IMPORTING
                                           equipment  TYPE equnr
                                           standorder TYPE daufn
                                           settlorder TYPE ilom_ordst.

    METHODS: z_equi_superior IMPORTING equnr TYPE equnr
                                       iwerk TYPE iwerk EXPORTING
                                       retorn TYPE c.

*&---------------------------------------------------------------------*
*& METHOD MODIFICAR EQUIPAMENTO                                        *
*&---------------------------------------------------------------------*
    METHODS: z_desinstal_equipamento IMPORTING
                                       equipment TYPE equnr
                                       funcloc   TYPE tplnr.

    METHODS: z_seleciona_local_tranf IMPORTING werks TYPE equz-iwerk
                                     EXPORTING local TYPE tplnr.

    METHODS: z_detalhes_equipamento IMPORTING
                                      equipment  TYPE equnr
                                    EXPORTING
                                      standorder TYPE daufn
                                      settlorder TYPE ilom_ordst
                                      costcenter TYPE ekostl.

    METHODS: z_status_equipamento  IMPORTING
                                     equipment TYPE equnr.
    "FF - 10/04/2024 #137726 - inicio
    METHODS: z_busca_imobilizado IMPORTING
                                   equipamento    TYPE equnr
                                   iwerk          TYPE iwerk
                                 EXPORTING
                                   imobilizado    TYPE anln1
                                   subimobilizado TYPE anln2.
    "FF - 10/04/2024 #137726 - fim
    METHODS: z_modificar_equipamento IMPORTING
                                       equipment      TYPE equnr
                                       maintplant     TYPE swerk
                                       bus_area       TYPE gsber
                                       planplant      TYPE iwerk
                                       costcenter     TYPE kostl
                                       work_ctr       TYPE num8
                                       standorder     TYPE daufn
                                       settlorder     TYPE ilom_ordst
                                       tplnr          TYPE tplnr
                                       imobilizado    TYPE anln1  "FF - 10/04/2024 #137726 -
                                       subimobilizado TYPE anln2.  "FF - 10/04/2024 #137726 -

    METHODS: z_modificar_equipamento_dev IMPORTING
                                           equipment      TYPE equnr
                                           maintplant     TYPE swerk
                                           bus_area       TYPE gsber
                                           planplant      TYPE iwerk
                                           costcenter     TYPE kostl
                                           work_ctr       TYPE num8
                                           standorder     TYPE daufn
                                           settlorder     TYPE ilom_ordst
                                           tplnr          TYPE tplnr
                                           imobilizado    TYPE anln1  "FF - 10/04/2024 #137726 -
                                           subimobilizado TYPE anln2.  "FF - 10/04/2024 #137726 -

    METHODS: z_instalar_equipamento IMPORTING
                                      equipment TYPE equnr
                                      swerk     TYPE swerk
                                      tplnr     TYPE tplnr.

    METHODS: z_instalar_equipamento_dev IMPORTING
                                          equipment TYPE equnr
                                          swerk     TYPE swerk
                                          tplnr     TYPE tplnr.
*&---------------------------------------------------------------------*
*& METHOD MODIFICAR PLANO DE MANUTENÇÃO                                *
*&---------------------------------------------------------------------*
    METHODS: z_selecionar_planos IMPORTING
                                   equipment TYPE equnr.

    METHODS: z_selecionar_planos_dev IMPORTING
                                       equipment TYPE equnr.

    METHODS: z_set_id_equipament  IMPORTING
                                    swerk        TYPE swerk
                                    equnr        TYPE equnr
                                  EXPORTING
                                    id_equipment TYPE num8.

    METHODS: z_set_id_equipament_dev  IMPORTING
                                        swerk        TYPE swerk
                                        tplnr        TYPE tplnr
                                      EXPORTING
                                        id_equipment TYPE num8.


    METHODS: z_modificar_planos IMPORTING
                                  equipment TYPE equnr
                                  swerk     TYPE iwerk
                                  gsber     TYPE gsber
                                  gewrk     TYPE num8
                                  tplnr     TYPE tplnr.
*                                  LAUFN     TYPE DAUFN.

    METHODS: z_modificar_planos_dev IMPORTING
                                      equipment TYPE equnr
                                      swerk     TYPE iwerk
                                      gsber     TYPE gsber
                                      gewrk     TYPE num8
                                      tplnr     TYPE tplnr.
*                                  TPLNR     TYPE TPLNR.
*                                  LAUFN     TYPE DAUFN.
*&---------------------------------------------------------------------*
*& ATRIBUTOS DA CLASSE                                                 *
*&---------------------------------------------------------------------*
  PRIVATE SECTION .
    DATA: it_methods      TYPE TABLE OF bapi_alm_order_method,
          it_header_up    TYPE TABLE OF bapi_alm_order_headers_up,
          it_header       TYPE TABLE OF bapi_alm_order_headers_i,
          it_operation    TYPE TABLE OF bapi_alm_order_operation,
          it_return       TYPE TABLE OF bapiret2,
          t_return        TYPE TABLE OF bapiret2,
          it_selc_plan    TYPE TABLE OF ty_selc_plan,
          it_notification TYPE STANDARD TABLE OF bapi2080_1.

    DATA: wa_methods            TYPE bapi_alm_order_method,
          wa_header             TYPE bapi_alm_order_headers_i,
          wa_operation          TYPE bapi_alm_order_operation,
          wa_data_general       TYPE bapi_itob,
          wa_data_generalx      TYPE bapi_itobx,
          wa_data_specific      TYPE bapi_itob_eq_only,
          wa_data_specificx     TYPE bapi_itob_eq_onlyx,
          wa_return_bapi_eqmt   TYPE bapireturn,
          wa_return             TYPE bapiret2,
          wa_notifheader_export TYPE bapi2080_nothdre,
          wa_notifheader        TYPE bapi2080_nothdri,
          wa_selc_plan          TYPE ty_selc_plan,
          wa_plko               TYPE plko,
          wa_sysstat            TYPE bapi2080_notsti,
          notif_type            TYPE char2.


    DATA: at_numero_ordem         TYPE char12,
          at_numero_nota          TYPE char12,
          at_numero_ordem_remonta TYPE char12,
          at_numero_ordem_abastec TYPE char12,
          at_id_equipment         TYPE num8.

    DATA: it_nota TYPE zpmt0020_t.

    DATA: short_text              TYPE char40.

ENDCLASS.                    "ZBAPIS DEFINITION

*&-------------------------------------------------------------------------------*
*& CLASS ZBAPIS IMPLEMENTATION                                                   *
*& AUTOR: ENIO JESUS                                                             *
*& 13.07.2015                                                                    *
*&-------------------------------------------------------------------------------*

CLASS zbapis IMPLEMENTATION.

* INICIAR IMPLEMENTAÇÃO DOS MÉTODOS *

*--------------------------------------------------------------------------------*
*                                                                                *
* MÉTODO PARA REALIZAR O EMPRÉSTIMO DO EQUIPAMENTO                               *
*                                                                                *
*--------------------------------------------------------------------------------*
  METHOD z_iniciar_processo_emprestimo.

    DATA: r_zuteis TYPE REF TO zuteis.
    CREATE OBJECT r_zuteis.

    LOOP AT it_saida_emprestimo_equi INTO wa_saida_emprestimo_equi.
      CLEAR: indice.
*      PERFORM f_lupa_ USING 'Iniciando processo de transferência' wa_saida_emprestimo_equi-equnr space.
      wa_saida_emprestimo_equi-equnr = |{ wa_saida_emprestimo_equi-equnr ALPHA = IN }|.

*-US 158036-26-11-2024-#158036-RJF-Início
      r_zuteis->z_bloqueio_custos_frotas( ).
      IF r_zuteis->gv_stop IS NOT INITIAL.
        me->gv_stopzb = r_zuteis->gv_stop.
        EXIT.
      ENDIF.
*-US 158036-26-11-2024-#158036-RJF-Fim

****************************/DETALHES DO EQUIPAMENTO\*****************************
*--------------------------------------------------------------------------------*

      z_detalhes_equipamento( EXPORTING equipment  = wa_saida_emprestimo_equi-equnr
                              IMPORTING standorder = at_data_general-standorder
                                        settlorder = at_data_general-settlorder
                                        costcenter = at_costcenter_origem ).


****************************/CRIAR CENTRO CUSTO DESTINO\**************************
*--------------------------------------------------------------------------------*

      r_zuteis->z_create_costcenter( swerk1 = tbx_centro_destino
                                     swerk2 = tbx_centro_destino
                                     center = at_costcenter_origem ).

*      R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-014 ).

*************************/VERIFICA SE EXISTE EQUIP SUPERIOR\**********************
*--------------------------------------------------------------------------------*
      z_equi_superior( EXPORTING equnr  = wa_saida_emprestimo_equi-equnr
                                 iwerk  = tbx_centro
                       IMPORTING retorn = wa_retorn ).



*************************/DESINSTALAR EQUIPAMENTO DA ORIGEM\**********************
*--------------------------------------------------------------------------------*

      IF wa_retorn IS INITIAL.
        z_desinstal_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                                 funcloc   = |{ tbx_centro }.FRO| ).
      ENDIF.

*-US 158036-11-12-2024-#158036-RJF-Início
      IF gw_erro IS NOT INITIAL.
        vg_erro = abap_true.
        EXIT.
      ENDIF.
*-US 158036-11-12-2024-#158036-RJF-Fim

*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*
      z_localizar_id_centro_trabalho( EXPORTING objty = 'A'
                                                werks = tbx_centro_destino
                                                arbpl = 'OFICINA'
                                      IMPORTING objid = id_cent_trabalho ).

*      z_set_id_equipament( EXPORTING        swerk = tbx_centro_destino
*                                            equnr = wa_saida_emprestimo_equi-equnr
*                           IMPORTING id_equipment = me->at_id_equipment ).
      "FF - 10/04/2024 #137726 - inicio
      z_busca_imobilizado( EXPORTING equipamento    = wa_saida_emprestimo_equi-equnr
                                     iwerk          = tbx_centro_destino
                           IMPORTING imobilizado    = imobilizado
                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim


      z_modificar_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                              maintplant = tbx_centro_destino
                              bus_area   = tbx_centro_destino
                              costcenter = at_costcenter_destino
                               planplant = tbx_centro_destino
                                work_ctr = id_cent_trabalho"me->at_id_equipment
                              standorder = me->at_numero_ordem_abastec
                              settlorder = me->at_numero_ordem_remonta
*                              tplnr      = zlocal_insta ). "FF - 22.11.2023 - del
                              tplnr      = loc_instalacao  "FF - 22.11.2023 - ins
                              imobilizado = imobilizado
                              subimobilizado = subimobilizado ).

*-US 158036-13-12-2024-#158036-RJF-Inicio
      IF gw_erro IS NOT INITIAL.
        EXIT.
      ENDIF.
*-US 158036-13-12-2024-#158036-RJF-Fim

*      R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-017 ).
*
**************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
**************************************************************************************
      SELECT SINGLE * FROM eqkt INTO @DATA(wa_eqkt) WHERE equnr =  @wa_saida_emprestimo_equi-equnr.
      SELECT SINGLE bukrs
           INTO @DATA(lv_empresa)
           FROM j_1bbranch
           WHERE branch EQ @tbx_centro_destino.
      DATA:v_texto TYPE ort01_anla.

      v_texto = wa_eqkt-eqktx(25).

*-US 142094-08-10-2024-#142094-RJF-Inicio
      SELECT SINGLE bukrs
           INTO @DATA(lv_empresaw)
           FROM j_1bbranch
           WHERE branch EQ @tbx_centro.
*-US 142094-08-10-2024-#142094-RJF-fim
*z_iniciar_processo_emprestimo.
      IF imobilizado IS NOT INITIAL.
        CALL FUNCTION 'Z_TRANSFERIR_IMOBILIZADO'
          EXPORTING
            imobilizado = imobilizado
            equnr       = wa_saida_emprestimo_equi-equnr
            kostl       = at_costcenter_destino
            shtxt       = v_texto
            bukrs       = lv_empresa
            gsber       = tbx_centro_destino "tbx_centro.
            bukrsw      = lv_empresaw.  "US 142094-08-10-2024-#142094-RJF

      ENDIF.
**************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
**************************************************************************************


************************/INSTALAR EQUIPAMENTO NO CENTRO DE DESTINO\*****************
*----------------------------------------------------------------------------------*


      z_instalar_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                                 swerk  = tbx_centro_destino
*                                 tplnr  = zlocal_insta )."FF - 22.11.2023 - del
                                 tplnr  = loc_instalacao ). "FF - 22.11.2023 - ins
      "FF - 10/04/2024 #137726 - inicio
      z_busca_imobilizado( EXPORTING equipamento    = wa_saida_emprestimo_equi-equnr
                                     iwerk          = tbx_centro_destino
                           IMPORTING imobilizado    = imobilizado
                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim

      z_modificar_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                             maintplant = tbx_centro_destino
                             bus_area   = tbx_centro_destino
                             costcenter = at_costcenter_destino
                              planplant = tbx_centro_destino
                               work_ctr = id_cent_trabalho"me->at_id_equipment
                             standorder = me->at_numero_ordem_abastec
                             settlorder = me->at_numero_ordem_remonta
*                                 tplnr  = zlocal_insta )."FF - 22.11.2023 - del
                                 tplnr  = loc_instalacao  "FF - 22.11.2023 - ins
                             imobilizado = imobilizado
                             subimobilizado = subimobilizado ).

      IF wa_saida_emprestimo_equi-cbx_ord_remon EQ 'X'.
*************************/ENCERRAR TODAS AS ORDENS DO EQUI\***********************
*--------------------------------------------------------------------------------*

        z_encerrar_todas_ordens( equipment  = wa_saida_emprestimo_equi-equnr
                                 standorder = at_data_general-standorder
                                 settlorder = at_data_general-settlorder ).




******************************/CRIAR ORDEM DE REMONTA\****************************
*--------------------------------------------------------------------------------*

        short_text = TEXT-029.

        z_criar_ordens_manutenc( order_type   = 'ZPM5'
                                 short_text   = short_text
                                 planplant    = tbx_centro_destino
*                                 funct_loc    = zlocal_insta  "FF - 22.11.2023 - del
                                 funct_loc    = loc_instalacao "FF - 22.11.2023 - ins
                                 bus_area     = tbx_centro_destino
                                 mn_wk_ctr    = 'OFICINA'
                                 plant        = tbx_centro_destino
                                 maintplant   = tbx_centro_destino
                                 loc_bus_area = tbx_centro_destino
                                 plangroup    = 'ABS'
                                 equipment    = wa_saida_emprestimo_equi-equnr
                                 costcenter   = at_costcenter_destino
                                 pmacttype    = 'Z03'
                                 priority     = '4'
                                 activity     = '0010'
                                 control_key  = 'PM01'
                                 description  = short_text
                                 create_notif = abap_true ).

        at_numero_ordem_remonta = z_criar_ordens_manutenc_get( ).

        IF at_numero_ordem_remonta IS INITIAL.
          MESSAGE 'Ordem de remonta não foi criada verificar centro de custo de destino' TYPE 'I' DISPLAY LIKE 'E'.
          CONTINUE.
        ENDIF.

      ENDIF.

*      R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-015 ).

**************************/CRIAR ORDEM DE ABASTECIMENTO\**************************
*--------------------------------------------------------------------------------*

      IF wa_saida_emprestimo_equi-cbx_ord_abast EQ 'X'.

        short_text = TEXT-030.
        z_criar_ordens_manutenc( order_type   = 'ZPM6'
                                 short_text   = short_text
                                 planplant    = tbx_centro_destino
*                                 funct_loc    = zlocal_insta  "FF - 22.11.2023 - del
                                 funct_loc    = loc_instalacao "FF - 22.11.2023 - ins
                                 bus_area     = tbx_centro_destino
                                 mn_wk_ctr    = 'OFICINA'
                                 plant        = tbx_centro_destino
                                 maintplant   = tbx_centro_destino
                                 loc_bus_area = tbx_centro_destino
                                 plangroup    = 'ABS'
                                 equipment    = wa_saida_emprestimo_equi-equnr
                                 costcenter   = at_costcenter_destino
                                 pmacttype    = 'Z11'
                                 priority     = '4'
                                 activity     = '0010'
                                 control_key  = 'PM01'
                                 description  = short_text
                                 create_notif = '' ).

        at_numero_ordem_abastec = z_criar_ordens_manutenc_get( ).

        IF at_numero_ordem_abastec IS INITIAL.
          MESSAGE 'Ordem de abastecimento não foi criada verificar centro de custo de destino' TYPE 'S' DISPLAY LIKE 'E'.
          CONTINUE.
        ENDIF.
*        R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-016 ).

      ENDIF.


*****************************/CRIAR NOTA DE EMPRÉSTIMO\***************************
*--------------------------------------------------------------------------------*
      IF wa_saida_emprestimo_equi-cbx_ord_abast IS NOT INITIAL.
        CONCATENATE TEXT-028 tbx_centro_destino INTO short_text SEPARATED BY space.

        z_localizar_id_centro_trabalho( EXPORTING objty = 'A'
                                                  werks = tbx_centro_destino
                                                  arbpl = 'OFICINA'
                                        IMPORTING objid = id_cent_trabalho ).
      ENDIF.

*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*
*      Z_SET_ID_EQUIPAMENT( EXPORTING        SWERK = TBX_CENTRO_DESTINO
*                                            EQ
*                           IMPORTING ID_EQUIPMENT = ME->AT_ID_EQUIPMENT ).
      "FF - 10/04/2024 #137726 - inicio
      z_busca_imobilizado( EXPORTING equipamento    = wa_saida_emprestimo_equi-equnr
                                     iwerk          = tbx_centro_destino
                           IMPORTING imobilizado    = imobilizado
                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim

      z_modificar_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                              maintplant = tbx_centro_destino
                              bus_area   = tbx_centro_destino
                              costcenter = at_costcenter_destino
                               planplant = tbx_centro_destino
                                work_ctr = id_cent_trabalho"ME->AT_ID_EQUIPMENT
                              standorder = me->at_numero_ordem_abastec
                              settlorder = me->at_numero_ordem_remonta
*                                 tplnr  = zlocal_insta )."FF - 22.11.2023 - del
                                 tplnr  = loc_instalacao  "FF - 22.11.2023 - ins
                                 imobilizado = imobilizado  "FF - 10/04/2024 #137726
                                 subimobilizado = subimobilizado ).  "FF - 10/04/2024 #137726

*-US 158036-13-12-2024-#158036-RJF-Inicio
      IF gw_erro IS NOT INITIAL.
        EXIT.
      ENDIF.
*-US 158036-13-12-2024-#158036-RJF-Fim

      z_criar_nota_emprestimo( equipment  = wa_saida_emprestimo_equi-equnr
                                 short_text = short_text
                                 priority   = '3'
                                 code_group = 'F0000010'
                                 coding     = '0070'
                                 notif_type = 'Z4'
                                 funct_loc  = |{ tbx_centro_destino }.{ 'FRO' }|
                                 plangroup  = 'FRO'
                                 maintplant = tbx_centro_destino
                                 planplant  = tbx_centro_destino
                                 pm_wkctr   = id_cent_trabalho
*                                 tplnr  = zlocal_insta )."FF - 22.11.2023 - del
                                 tplnr  = loc_instalacao ). "FF - 22.11.2023 - ins


************************/MODIFICAR OS PLANOS DO EQUIPAMENTO\************************
*----------------------------------------------------------------------------------*
      IF wa_saida_emprestimo_equi-cbx_ord_abast IS NOT INITIAL.
        z_selecionar_planos( equipment = wa_saida_emprestimo_equi-equnr ).

        IF NOT it_selc_plan IS INITIAL.

          z_modificar_planos( equipment = wa_saida_emprestimo_equi-equnr
                                  swerk = tbx_centro_destino
                                  gsber = tbx_centro_destino
                                  gewrk = id_cent_trabalho
*                                  tplnr = zlocal_insta ). "ME->AT_ID_EQUIPMENT ).
                                  tplnr = loc_instalacao ). "FF - 22.11.2023 - ins

*                                TPLNR = |{ TBX_CENTRO_DESTINO }.{ 'FRO' }| ).
*                                LAUFN = ME->AT_NUMERO_ORDEM_ABASTEC ).

          r_zuteis->z_atualiza_status_bapis(
            txt_status = TEXT-018 ).
        ENDIF.
      ENDIF.



*********************/INSERIR DADOS DO EMPRÉSTIMO EM TABELA Z\**********************
*----------------------------------------------------------------------------------*
      IF wa_saida_emprestimo_equi-cbx_ord_abast IS NOT INITIAL.
        IF p_temp EQ abap_true.

          "FF - 10/04/2024 #137726 - inicio
          z_busca_imobilizado( EXPORTING equipamento    = wa_saida_emprestimo_equi-equnr
                                         iwerk          = tbx_centro_destino
                               IMPORTING imobilizado    = imobilizado
                                         subimobilizado = subimobilizado ).
          "FF - 10/04/2024 #137726 - fim

          r_zuteis->z_insert_dados_emprestimo(
            equnr          = wa_saida_emprestimo_equi-equnr
            eqktx          = wa_saida_emprestimo_equi-eqktx
            swerk          = wa_data_general-maintplant
            iwerk          = tbx_centro_destino
            qt_dias        = tbx_qt_dias
            erdat          = sy-datum
            uname          = sy-uname
            numero_nota    = wa_notifheader_export-notif_no
            ordem_abast    = me->at_numero_ordem_abastec
            ordem_remon    = me->at_numero_ordem_remonta
            cent_origem    = tbx_centro
            local_origem   = me->zlocal_origem
            nota_zpmt5     = ''
            imobilizado    = imobilizado  "FF - 10/04/2024 #137726
            subimobilizado = subimobilizado "FF - 10/04/2024 #137726
            devolucao_automatica = wa_saida_emprestimo_equi-devolucao_automatica )." Rubenilson - 24.12.24 - US138088

          DELETE it_saida_equi_disponiveis WHERE equnr = wa_saida_emprestimo_equi-equnr.
        ENDIF.
      ENDIF.
      CLEAR: id_cent_trabalho, at_costcenter_destino, at_costcenter_origem, at_numero_ordem_remonta, at_numero_ordem_abastec,
             at_data_general-standorder, at_data_general-settlorder,
             me->at_numero_ordem.


*** Stefanini - IR198570 - 19/09/2024 - LAZAROSR - Início de Alteração
*      CLEAR: TBX_CENTRO_DESTINO.
*** Stefanini - IR198570 - 19/09/2024 - LAZAROSR - Fim de Alteração

    ENDLOOP.

    wa_stable-col = 'X'.
    wa_stable-row = 'X'.
    CALL METHOD obj_alv_0200->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

*    R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-019  ).



  ENDMETHOD.                    "CREATE_ZBAPIS

*--------------------------------------------------------------------------------*
*                                                                                *
* MÉTODO PARA REALIZAR A DEVOLUÇÃO DO EQUIPAMENTO                                *
*                                                                                *
*--------------------------------------------------------------------------------*
  METHOD z_iniciar_processo_devolucao.

    CLEAR: vg_erro.
    LOOP AT it_saida_dev_equi INTO wa_saida_dev_equi.


      wa_saida_dev_equi-equnr = |{ wa_saida_dev_equi-equnr ALPHA = IN }|.

*      PERFORM f_lupa USING 'Iniciando devolução' wa_saida_dev_equi-equnr space.
      DATA: r_zuteis                      TYPE REF TO zuteis,
            r_atualiza_tela_pos_devolucao TYPE REF TO z_seleciona_dados.
      wa_saida_dev_equi-equnr = |{ wa_saida_dev_equi-equnr ALPHA = IN }|.

      CREATE OBJECT: r_atualiza_tela_pos_devolucao,
                         r_zuteis.

*-US 158036-11-12-2024-#158036-RJF-Início
      IF wa_saida_emprestimo_equi-equnr IS INITIAL.
        wa_saida_emprestimo_equi-equnr = wa_saida_dev_equi-equnr.
      ENDIF.
      r_zuteis->z_bloqueio_custos_frotas( ).

      IF r_zuteis->gv_stop IS NOT INITIAL.
        vg_erro = r_zuteis->gv_stop.
        EXIT.
      ENDIF.
*-US 158036-11-12-2024-#158036-RJF-Fim

      SELECT SINGLE *
      FROM zequi_emprestimo
      INTO @DATA(zequi_emprestimo)
        WHERE equnr EQ @wa_saida_dev_equi-equnr.


      IF zequi_emprestimo-local_origem IS NOT INITIAL.
*      CLEAR: zlocal_insta.
        me->zlocal_insta = zequi_emprestimo-local_origem.
      ELSE.
        IF me->zlocal_insta IS INITIAL.
          z_seleciona_local_tranf(
            EXPORTING
              werks = wa_saida_dev_equi-cent_origem
            IMPORTING
              local = me->zlocal_insta
          ).

        ENDIF.
      ENDIF.

      IF me->zlocal_insta IS INITIAL.
        MESSAGE i024(sd) WITH 'Não existe local de intalação tipo ' 'Y cadastrado, não é possivel realizar devolução.'.
        vg_erro = 'X'.
        EXIT.
      ENDIF.



****************************/DETALHES DO EQUIPAMENTO\*****************************
*--------------------------------------------------------------------------------*

      z_detalhes_equipamento( EXPORTING equipment  = wa_saida_dev_equi-equnr
                              IMPORTING costcenter = at_costcenter_origem ).


****************************/CRIAR CENTRO CUSTO DESTINO\**************************
*--------------------------------------------------------------------------------*
      r_zuteis->z_create_costcenter_dev( swerk1 = wa_saida_dev_equi-cent_origem
                                         swerk2 = wa_saida_dev_equi-cent_origem
                                         center = at_costcenter_origem ).



*************************/DESINSTALAR EQUIPAMENTO DA ORIGEM\**********************
*--------------------------------------------------------------------------------*

      z_desinstal_equipamento( equipment = wa_saida_dev_equi-equnr
                               funcloc   = |{ wa_saida_dev_equi-iwerk }.FRO | ).

*-US 158036-11-12-2024-#158036-RJF-Início
      IF gw_erro IS NOT INITIAL.
        vg_erro = abap_true.
        EXIT.
      ENDIF.
*-US 158036-11-12-2024-#158036-RJF-Fim


*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*
      z_localizar_id_centro_trabalho( EXPORTING objty = 'A'
                                                werks = wa_saida_dev_equi-cent_origem
                                                arbpl = 'OFICINA'
                                      IMPORTING objid = id_cent_trabalho ).



*
*      z_set_id_equipament_dev( EXPORTING swerk        = wa_saida_dev_equi-cent_origem
*                           IMPORTING id_equipment     = me->at_id_equipment ).
      "FF - 10/04/2024 #137726 - inicio
      z_busca_imobilizado( EXPORTING equipamento    = wa_saida_dev_equi-equnr
                                     iwerk          = wa_saida_dev_equi-cent_origem
                           IMPORTING imobilizado    = imobilizado
                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim

      z_modificar_equipamento_dev( equipment      = wa_saida_dev_equi-equnr
                                   maintplant     = wa_saida_dev_equi-cent_origem
                                   bus_area       = wa_saida_dev_equi-cent_origem
                                   costcenter     = at_costcenter_destino
                                   planplant      = wa_saida_dev_equi-cent_origem
                                   work_ctr       = id_cent_trabalho "me->at_id_equipment
                                   standorder     = ''
                                   settlorder     = ''
                                   tplnr          = me->zlocal_insta
                                   imobilizado    = imobilizado  "FF - 10/04/2024 #137726
                                   subimobilizado = subimobilizado ).  "FF - 10/04/2024 #137726


***********************/INSTALAR EQUIPAMENTO NO LOCAL ORIGEM\***********************
*----------------------------------------------------------------------------------*
      z_instalar_equipamento_dev( equipment = wa_saida_dev_equi-equnr
                                  swerk     = wa_saida_dev_equi-cent_origem
                                  tplnr     = me->zlocal_insta ).

      "FF - 10/04/2024 #137726 - inicio
      z_busca_imobilizado( EXPORTING equipamento    = wa_saida_dev_equi-equnr
                                     iwerk          = wa_saida_dev_equi-cent_origem
                           IMPORTING imobilizado    = imobilizado
                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim
*************************************************************************************
*Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
*************************************************************************************
      SELECT SINGLE * FROM eqkt INTO @DATA(wa_eqkt) WHERE equnr =  @wa_saida_emprestimo_equi-equnr.
      DATA: v_texto TYPE ort01_anla.

      v_texto = wa_eqkt-eqktx(25).
      SELECT SINGLE bukrs
           INTO @DATA(lv_empresa)
           FROM j_1bbranch
           WHERE branch EQ @tbx_centro_destino.

*-US 142094-08-10-2024-#142094-RJF-Inicio
      SELECT SINGLE bukrs
           INTO @DATA(lv_empresaw)
           FROM j_1bbranch
           WHERE branch EQ @tbx_centro.
*-US 142094-08-10-2024-#142094-RJF-fim

      IF imobilizado IS NOT INITIAL.
        CALL FUNCTION 'Z_TRANSFERIR_IMOBILIZADO'
          EXPORTING
            imobilizado = imobilizado
            equnr       = wa_saida_dev_equi-equnr
            kostl       = at_costcenter_destino
            shtxt       = v_texto
            bukrs       = lv_empresa
            gsber       = tbx_centro_destino
            bukrsw      = lv_empresaw.  "US 142094-08-10-2024-#142094-RJF
      ENDIF.
*************************************************************************************
*Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
*************************************************************************************

      z_modificar_equipamento_dev( equipment      = wa_saida_dev_equi-equnr
                                   maintplant     = wa_saida_dev_equi-cent_origem
                                   bus_area       = wa_saida_dev_equi-cent_origem
                                   costcenter     = at_costcenter_destino
                                   planplant      = wa_saida_dev_equi-cent_origem
                                   work_ctr       = id_cent_trabalho "me->at_id_equipment
                                   standorder     = ''
                                   settlorder     = ''
                                   tplnr          = me->zlocal_insta
                                   imobilizado    = imobilizado  "FF - 10/04/2024 #137726
                                   subimobilizado = subimobilizado ).  "FF - 10/04/2024 #137726


******************************/CRIAR ORDEM DE REMONTA\****************************
*--------------------------------------------------------------------------------*
      IF zequi_emprestimo IS NOT INITIAL.


*************************/ENCERRAR TODAS AS ORDENS DO EQUI\***********************
*--------------------------------------------------------------------------------*


        short_text = TEXT-029.
        z_criar_ordens_manutenc_dev( order_type   = 'ZPM5'
                                     short_text   = short_text
                                     planplant    = wa_saida_dev_equi-cent_origem
                                     bus_area     = wa_saida_dev_equi-cent_origem
                                     funct_loc    = zlocal_insta
                                     mn_wk_ctr    = 'OFICINA'
                                     plant        = wa_saida_dev_equi-cent_origem
                                     maintplant   = wa_saida_dev_equi-cent_origem
                                     loc_bus_area = wa_saida_dev_equi-cent_origem
                                     plangroup    = 'ABS'
                                     equipment    = wa_saida_dev_equi-equnr
                                     costcenter   = at_costcenter_destino
                                     pmacttype    = 'Z03'
                                     priority     = '4'
                                     activity     = '0010'
                                     control_key  = 'PM01'
                                     description  = short_text
                                     create_notif = abap_true ).

        at_numero_ordem_remonta = z_criar_ordens_manutenc_get_dv( ).


**************************/CRIAR ORDEM DE ABASTECIMENTO\**************************
*--------------------------------------------------------------------------------*

        IF wa_saida_dev_equi-standorder IS NOT INITIAL.
          short_text = TEXT-030.
          z_criar_ordens_manutenc_dev( order_type   = 'ZPM6'
                                       short_text   = short_text
                                       planplant    = wa_saida_dev_equi-cent_origem
                                       bus_area     = wa_saida_dev_equi-cent_origem
                                       funct_loc    = zlocal_insta
                                       mn_wk_ctr    = 'OFICINA'
                                       plant        = wa_saida_dev_equi-cent_origem
                                       maintplant   = wa_saida_dev_equi-cent_origem
                                       loc_bus_area = wa_saida_dev_equi-cent_origem
                                       plangroup    = 'ABS'
                                       equipment    = wa_saida_dev_equi-equnr
                                       costcenter   = at_costcenter_destino
                                       pmacttype    = 'Z11'
                                       priority     = '4'
                                       activity     = '0010'
                                       control_key  = 'PM01'
                                       description  = short_text
                                       create_notif = ' ' ).

          at_numero_ordem_abastec = z_criar_ordens_manutenc_get_dv( ).
        ENDIF.

        IF zequi_emprestimo-standorder IS NOT INITIAL
         OR zequi_emprestimo-settlorder IS NOT INITIAL.
          z_encerrar_todas_ordens( equipment  = wa_saida_dev_equi-equnr
                                   standorder = wa_saida_dev_equi-standorder
                                   settlorder = wa_saida_dev_equi-settlorder ).
        ENDIF.
      ENDIF.

*    LOOP AT IT_SAIDA_EQUI_RESPONSAVEL INTO WA_SAIDA_EQUI_RESPONSAVEL WHERE
*      CBX_DEVOLVER = 'X'.




****************************/SELECIONA LOCAL DE TRANSFERENCIA\*****************************
*--------------------------------------------------------------------------------*




****************************/DELETAR DA TABELA DE EMPRESTIMO O EQUIPAMENTO**************************
*--------------------------------------------------------------------------------*
      IF zequi_emprestimo IS NOT INITIAL.
        DELETE FROM zequi_emprestimo WHERE equnr = wa_saida_dev_equi-equnr.
        COMMIT WORK.
        WAIT UP TO 02 SECONDS.
      ENDIF.





*      CALL FUNCTION 'BUFFER_SVR_REFRESH'
*        EXPORTING
*          pi_buffer_svr_refresh = abap_true.






***********************/INSTALAR EQUIPAMENTO NO LOCAL ORIGEM\***********************
*----------------------------------------------------------------------------------*

*    Z_INSTALAR_EQUIPAMENTO( EQUIPMENT = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR
*                                SWERK = WA_SAIDA_EQUI_RESPONSAVEL-SWERK ).
**    ENDLOOP.




*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*

*      z_set_id_equipament_dev( EXPORTING swerk        = wa_saida_dev_equi-cent_origem
*                           IMPORTING id_equipment     = me->at_id_equipment ).
      "FF - 10/04/2024 #137726 - inicio
      z_busca_imobilizado( EXPORTING equipamento    = wa_saida_dev_equi-equnr
                                     iwerk          = wa_saida_dev_equi-cent_origem
                           IMPORTING imobilizado    = imobilizado
                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim

      z_modificar_equipamento_dev( equipment      = wa_saida_dev_equi-equnr
                                   maintplant     = wa_saida_dev_equi-cent_origem
                                   bus_area       = wa_saida_dev_equi-cent_origem
                                   costcenter     = at_costcenter_destino
                                   planplant      = wa_saida_dev_equi-cent_origem
                                   work_ctr       = id_cent_trabalho "me->at_id_equipment
                                   standorder     = me->at_numero_ordem_abastec
                                   settlorder     = me->at_numero_ordem_remonta
                                   tplnr          = me->zlocal_insta
                                   imobilizado    = imobilizado  "FF - 10/04/2024 #137726
                                   subimobilizado = subimobilizado ).  "FF - 10/04/2024 #137726



************************/MODIFICAR OS PLANOS DO EQUIPAMENTO\************************
*----------------------------------------------------------------------------------*

      z_selecionar_planos_dev( equipment = wa_saida_dev_equi-equnr ).
      IF ( NOT it_selc_plan IS INITIAL ).

        z_modificar_planos_dev( equipment = wa_saida_dev_equi-equnr
                                swerk     = wa_saida_dev_equi-cent_origem
                                gsber     = wa_saida_dev_equi-cent_origem
                                gewrk     = id_cent_trabalho
                                tplnr     = me->zlocal_insta ). "me->at_id_equipment ).
*                              LAUFN = ME->AT_NUMERO_ORDEM_ABASTEC ).
      ENDIF.


****************************/ENCERRAR NOTA DE EMPRÉSTIMO**************************
*--------------------------------------------------------------------------------*
      IF zequi_emprestimo-notif_no IS NOT INITIAL.
        z_close_nota_emprestimo( notif_no = wa_saida_dev_equi-notif_no
                                 refdate  = sy-datum
                                 reftime  = sy-uzeit ).
      ENDIF.

*----------------------------------------------------------------------------------*

      DELETE it_saida_equi_responsavel WHERE equnr = wa_saida_dev_equi-equnr.
      COMMIT WORK.

      CLEAR: at_id_equipment, at_costcenter_origem, at_costcenter_destino, wa_data_general, zequi_emprestimo, at_numero_ordem_remonta, at_numero_ordem_abastec,
             me->at_numero_ordem, wa_saida_dev_equi.
    ENDLOOP.

  ENDMETHOD.                    "Z_INICIAR_PROCESSO_DEVOLUCAO

**********************************************************************************
*& Descrição: Localizar ID centro de trabalho responsavel                       &*
**********************************************************************************
  METHOD z_localizar_id_centro_trabalho.

    CLEAR: id_cent_trabalho.

    SELECT SINGLE *
    FROM crhd
    INTO @DATA(_crhd)
    WHERE objty EQ @objty
      AND werks EQ @werks
      AND arbpl EQ @arbpl.

    IF _crhd IS NOT INITIAL.
      id_cent_trabalho = _crhd-objid.
    ENDIF.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Localiza centro de trabalho'
                  filial_origem   = werks
                  filial_destino  = werks
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.

**********************************************************************************
*& Descrição: Criar nota de empréstimo de equipamento                           &*
*& Atributo.: WA_NOTIFHEADER, WA_NOTIFHEADER_EXPORT, IT_RETURN, WA_RETURN       &*                                                      *
*& Parâmetro: EQUIPMENT, SHORT_TEXT, PRIORITY, CODE_GROUP, CODING, NOTIF_TYPE   &*
**********************************************************************************
  METHOD z_criar_nota_emprestimo.

    CLEAR: it_return, wa_notifheader, wa_return.


    wa_notifheader-funct_loc  = tplnr.
    wa_notifheader-equipment  = equipment.
    wa_notifheader-short_text = short_text.
*    WA_NOTIFHEADER-PRIORITY   = PRIORITY.
    wa_notifheader-code_group = code_group.
    wa_notifheader-coding     = coding.
    wa_notifheader-maintplant = maintplant.
    wa_notifheader-planplant  = planplant.
    wa_notifheader-plangroup  = plangroup.
    wa_notifheader-pm_wkctr   = pm_wkctr.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE'
      EXPORTING
        notif_type         = notif_type
        notifheader        = wa_notifheader
        task_determination = ' '
      IMPORTING
        notifheader_export = wa_notifheader_export
      TABLES
        return             = it_return.

    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING
        number      = wa_notifheader_export-notif_no
      IMPORTING
        notifheader = wa_notifheader_export
      TABLES
        return      = it_return.

    CLEAR: wa_return.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 2 SECONDS.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Criar nota emprestimo'
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_NOTIF_CREATE

  METHOD: z_close_nota_emprestimo.
    CLEAR: wa_sysstat, wa_return.

    wa_sysstat-langu    = sy-langu.
    wa_sysstat-languiso = sy-langu.
    wa_sysstat-refdate  = refdate.
    wa_sysstat-reftime  = reftime.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
      EXPORTING
        number   = notif_no
        syststat = wa_sysstat
      TABLES
        return   = it_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 2 SECONDS.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'fechar nota emprestimo devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


    IF it_zpmt0079 IS NOT INITIAL.
      MODIFY zpmt0079 FROM TABLE it_zpmt0079.
      COMMIT WORK.
    ENDIF.
*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_CLOSE_NOTA_EMPRESTIMO

  METHOD: z_modify_nota_emprestimo.

    DATA: lt_item_mod         TYPE TABLE OF bapi2080_notitemi,
          lt_item_modx        TYPE eaml_bapi2080_notitemi_x_t,
          lt_caus_mod         TYPE TABLE OF bapi2080_notcausi,
          lt_caus_modx        TYPE alm_me_bapi2080_notcausi_x_t,
          lt_fact_mod         TYPE TABLE OF bapi2080_notactvi,
          lt_fact_modx        TYPE alm_me_bapi2080_notactvi_x_t,
          _notifheader_export TYPE bapi2080_nothdre,
          t_return            TYPE bapiret2_t.


    CLEAR: wa_sysstat, wa_return.
    FREE: t_return .

    DATA(_notifheader) =
    VALUE bapi2080_nothdri(
                            funct_loc    = tplnr
*                            equipment    = equipment
                            maintplant   = planplant
                            planplant    = planplant
).

    DATA(_notifheader_x) =
    VALUE bapi2080_nothdri_x(
                              funct_loc    = abap_true
*                              equipment    = abap_true
                              maintplant   = abap_true
                              planplant    = abap_true

    ).


    CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
      EXPORTING
        number             = notif_no
        notifheader        = _notifheader
        notifheader_x      = _notifheader_x
      IMPORTING
        notifheader_export = _notifheader_export
      TABLES
        notifitem          = lt_item_mod
        notifitem_x        = lt_item_modx
        notifcaus          = lt_caus_mod
        notifcaus_x        = lt_caus_modx
        notifactv          = lt_fact_mod
        notifactv_x        = lt_fact_mod
        return             = t_return.
    IF NOT line_exists( t_return[ type = 'E' ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = wa_return.
      WAIT UP TO 2 SECONDS.
    ELSE.
    ENDIF.

    CLEAR: _notifheader, _notifheader_x, wa_return.
  ENDMETHOD.                    "Z_CLOSE_NOTA_EMPRESTIMO

**********************************************************************************
*& Descrição: Criar novas ordens de manutenção com centro do destino            &*
*& Atributo.: WA_HEADER, WA_OPERATION, IT_HEADER, IT_OPERATION, IT_RETURN,      &*
*&            WA_RETURN,                                                        &*                                                      *
*& Parâmetro: EQUIPMENT, ORDER_TYPE, SHORT_TEXT, PLANPLANT, BUS_AREA, PLANT     &*
*&            MN_WK_CTR, LOC_BUS_AREA, PLANGROUP, COSTCENTER, PRIORITY          &*
*&            ACTIVITY, CONTROL_KEY, DESCRIPTION, AT_NUMERO_ORDEM               &*
**********************************************************************************
  METHOD z_criar_ordens_manutenc.


    DATA: lv_orderid      TYPE aufnr,
          lv_refnum       TYPE ifrefnum,
          lv_oper_no      TYPE objidext,
          ls_iloa         TYPE iloa,
          numero_ordem_v2 TYPE char1,
          lv_empresa      TYPE bukrs,
          _method         TYPE swo_method,
          at_orderid      TYPE aufnr VALUE 1,
          at_refnum       TYPE ifrefnum VALUE 1,
          at_seqopr       TYPE ifrefnum,
          at_oper_no      TYPE objidext.



    FREE: it_methods[], it_header[], it_operation[], it_return[], it_header_up.
    CLEAR: wa_return, at_numero_ordem, wa_header, wa_notifheader_export, notif_type, me->at_numero_nota, wa_notifheader_export.

    DATA: it_nota TYPE zpmt0020_t.

    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - início / AOENNING
    IF create_notif EQ abap_true.

      CLEAR: id_cent_trabalho.
      me->z_localizar_id_centro_trabalho( EXPORTING objty = 'A'
                                                    werks = planplant
                                                    arbpl = 'OFICINA'
                                          IMPORTING objid = id_cent_trabalho ).

      wa_notifheader-funct_loc  = funct_loc.
      wa_notifheader-equipment  = equipment.
      wa_notifheader-short_text = short_text.
      wa_notifheader-priority   = '4'.
      wa_notifheader-code_group = 'F0000010'.
      wa_notifheader-coding     = '0020'.
      wa_notifheader-reportedby = sy-uname.
      wa_notifheader-maintplant = maintplant.
      wa_notifheader-planplant  = planplant.
      wa_notifheader-plangroup  = plangroup.
      wa_notifheader-pm_wkctr   = id_cent_trabalho.
      notif_type = 'Z9'.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      FREE: it_return.
      CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          notif_type         = notif_type
          notifheader        = wa_notifheader
          task_determination = ' '
        IMPORTING
          notifheader_export = wa_notifheader_export
        TABLES
          return             = it_return.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      FREE: it_return.
      CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          number      = wa_notifheader_export-notif_no
        IMPORTING
          notifheader = wa_notifheader_export
        TABLES
          return      = it_return.

      CLEAR: wa_return.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = wa_return.
      WAIT UP TO 2 SECONDS.

    ENDIF.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim


    at_orderid = |{ at_orderid ALPHA = IN }|.
    at_refnum  = |{ at_refnum  ALPHA = IN }|.

    at_orderid+0(1) = '%'.
    _method = 'CREATE'.

    at_oper_no = at_orderid.
    at_oper_no+12(4) = '0010'.

    it_methods = VALUE bapi_alm_order_method_t( ( refnumber = at_refnum objecttype = 'HEADER'    method = _method  objectkey  = at_orderid ) ).
    APPEND VALUE #( refnumber = at_refnum objecttype = 'OPERATION' method = _method  objectkey  = at_oper_no ) TO it_methods.
    APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE' objectkey = at_orderid ) TO it_methods.
    APPEND VALUE #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = at_orderid ) TO it_methods.

    IF order_type = 'ZPM5'.
      CLEAR it_methods[].
      CONCATENATE at_orderid wa_notifheader_export-notif_no INTO DATA(at_notif_no).
      APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'CREATETONOTIF'  objectkey = at_notif_no ) TO it_methods.
      APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE'        objectkey = at_orderid  ) TO it_methods.
      APPEND VALUE #( refnumber = at_refnum objecttype = ''          method = 'SAVE'           objectkey = at_notif_no ) TO it_methods.

      wa_header-orderid = at_orderid.
      wa_header-notif_type = 'Z9'.

      APPEND VALUE #( orderid  = at_orderid
                      notif_no = abap_true )
                TO it_header_up.

    ENDIF.

    SELECT SINGLE bukrs
      INTO lv_empresa
      FROM j_1bbranch
      WHERE branch EQ planplant.

*    wa_header-orderid       = lv_orderid.
    wa_header-order_type    = order_type.
    wa_header-funct_loc     = funct_loc.
    wa_header-short_text    = short_text.
    wa_header-planplant     = planplant.
    wa_header-loc_comp_code = lv_empresa.
    wa_header-bus_area      = bus_area.
    wa_header-mn_wk_ctr     = mn_wk_ctr.
    wa_header-plant         = plant.
    wa_header-maintplant    = maintplant.
    wa_header-loc_bus_area  = loc_bus_area.
    wa_header-plangroup     = plangroup.
    wa_header-equipment     = equipment.
    wa_header-costcenter    = costcenter.
    wa_header-start_date    = sy-datum.
    wa_header-priority      = priority.
    APPEND wa_header TO it_header.
    CLEAR wa_header.

    wa_operation-activity    = activity.
    wa_operation-control_key = control_key.
    wa_operation-description = description.
    APPEND wa_operation TO it_operation.
    CLEAR wa_operation.

    FREE: it_return.
    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods   = it_methods
        it_header    = it_header
        it_header_up = it_header_up
        it_operation = it_operation
        return       = it_return.

    CLEAR: wa_return.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 02 SECONDS.

    READ TABLE it_return INTO wa_return WITH KEY number = '112'.

    "14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / aoenning
    IF wa_return-message_v2 IS INITIAL.
      READ TABLE it_return INTO wa_return WITH KEY number = '126'.
    ENDIF.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim

    CLEAR: me->at_numero_ordem.
    me->at_numero_ordem = wa_return-message_v2.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = me->at_numero_ordem
      IMPORTING
        output = me->at_numero_ordem.

    CLEAR: me->at_numero_nota.
    me->at_numero_nota = wa_notifheader_export-notif_no.


*   Verificar local instalação da ordem.
    SELECT SINGLE *
    FROM iloa AS a
    INNER JOIN afih AS b ON b~iloan EQ a~iloan
    INTO CORRESPONDING FIELDS OF ls_iloa
     WHERE b~aufnr EQ at_numero_ordem.


    IF ls_iloa-tplnr IS NOT INITIAL AND ls_iloa-tplnr NE funct_loc.
      PERFORM shdb_mod_ordem USING funct_loc me->at_numero_ordem.

      IF wa_notifheader_export-notif_no IS NOT INITIAL.
        me->z_modify_nota_emprestimo(
          EXPORTING
            equipment = equipment
            planplant = planplant
            tplnr     = funct_loc
            notif_no  = wa_notifheader_export-notif_no
        ).
      ENDIF.
    ENDIF.

    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / aoenning
    IF create_notif EQ abap_true.
      "Preenche o numero da ordem na operação
      DATA: lt_oprol3 TYPE TABLE OF bapi_alm_olist_relation,
            lt_return TYPE TABLE OF bapiret2.

      FREE: it_return.
      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
        EXPORTING
          number   = me->at_numero_ordem
        TABLES
          et_oprol = lt_oprol3
          return   = lt_return.

      CALL FUNCTION 'ZPM_ATUALIZA_OBJETOS_ORDEM' IN BACKGROUND TASK AS SEPARATE UNIT
        EXPORTING
          i_aufnr = me->at_numero_ordem
          i_notas = it_nota "wa_notifheader_export-notif_no
        TABLES
          i_oprol = lt_oprol3.

    ENDIF.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim


*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Criar ordem manutenção '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_ORDER_MAINTAIN

*  **********************************************************************************
*& Descrição: Verifica nota de manutenção vinculada a ordem&*
*& Atributo.: WA_HEADER, WA_OPERATION, IT_HEADER, IT_OPERATION, IT_RETURN,      &*
*&            WA_RETURN,                                                        &*                                                      *
*& Parâmetro: EQUIPMENT, ORDER_TYPE, SHORT_TEXT, PLANPLANT, BUS_AREA, PLANT     &*
*&            MN_WK_CTR, LOC_BUS_AREA, PLANGROUP, COSTCENTER, PRIORITY          &*
*&            ACTIVITY, CONTROL_KEY, DESCRIPTION, AT_NUMERO_ORDEM               &*
**********************************************************************************
  METHOD  z_get_data_ordem.

    "Verifica se ordem de nota vinculada.
    SELECT SINGLE * FROM viaufkst
      INTO ws_data_ordem
      WHERE aufnr EQ aufnr.

  ENDMETHOD.
*  **********************************************************************************
*& Descrição: Criar novas ordens de manutenção com centro do destino            &*
*& Atributo.: WA_HEADER, WA_OPERATION, IT_HEADER, IT_OPERATION, IT_RETURN,      &*
*&            WA_RETURN,                                                        &*                                                      *
*& Parâmetro: EQUIPMENT, ORDER_TYPE, SHORT_TEXT, PLANPLANT, BUS_AREA, PLANT     &*
*&            MN_WK_CTR, LOC_BUS_AREA, PLANGROUP, COSTCENTER, PRIORITY          &*
*&            ACTIVITY, CONTROL_KEY, DESCRIPTION, AT_NUMERO_ORDEM               &*
**********************************************************************************
  METHOD z_criar_ordens_manutenc_dev.

    FREE: it_methods, it_header, it_operation, it_return.
    CLEAR: wa_return, wa_header, wa_operation, wa_methods.

    DATA: lv_orderid      TYPE aufnr,
          lv_refnum       TYPE ifrefnum,
          lv_oper_no      TYPE objidext,
          contnum         TYPE p DECIMALS 2,
          ls_iloa         TYPE iloa,
          numero_ordem_v2 TYPE char10,
          lv_empresa      TYPE bukrs,
          _method         TYPE swo_method,
          at_orderid      TYPE aufnr VALUE 1,
          at_refnum       TYPE ifrefnum VALUE 1,
          at_seqopr       TYPE ifrefnum,
          at_oper_no      TYPE objidext.



    FREE: it_methods[], it_header[], it_operation[], it_return[], it_header_up.
    CLEAR: wa_return, at_numero_ordem, wa_header, wa_notifheader_export, notif_type, me->at_numero_nota, wa_notifheader_export.


    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - início / AOENNING
    IF create_notif EQ abap_true.

      CLEAR: id_cent_trabalho.
      me->z_localizar_id_centro_trabalho( EXPORTING objty = 'A'
                                                    werks = planplant
                                                    arbpl = 'OFICINA'
                                          IMPORTING objid = id_cent_trabalho ).

      wa_notifheader-funct_loc  = funct_loc.
      wa_notifheader-equipment  = equipment.
      wa_notifheader-short_text = short_text.
      wa_notifheader-priority   = '4'.
      wa_notifheader-code_group = 'F0000010'.
      wa_notifheader-coding     = '0020'.
      wa_notifheader-reportedby = sy-uname.
      wa_notifheader-maintplant = maintplant.
      wa_notifheader-planplant  = planplant.
      wa_notifheader-plangroup  = plangroup.
      wa_notifheader-pm_wkctr   = id_cent_trabalho.
      notif_type = 'Z9'.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      FREE: it_return.
      CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          notif_type         = notif_type
          notifheader        = wa_notifheader
          task_determination = ' '
        IMPORTING
          notifheader_export = wa_notifheader_export
        TABLES
          return             = it_return.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      FREE: it_return.
      CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          number      = wa_notifheader_export-notif_no
        IMPORTING
          notifheader = wa_notifheader_export
        TABLES
          return      = it_return.

      CLEAR: wa_return.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = wa_return.
      WAIT UP TO 2 SECONDS.

    ENDIF.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim

    at_orderid = |{ at_orderid ALPHA = IN }|.
    at_refnum  = |{ at_refnum  ALPHA = IN }|.

    at_orderid+0(1) = '%'.
    _method = 'CREATE'.

    at_oper_no = at_orderid.
    at_oper_no+12(4) = '0010'.

    it_methods = VALUE bapi_alm_order_method_t( ( refnumber = at_refnum objecttype = 'HEADER'    method = _method  objectkey  = at_orderid ) ).
    APPEND VALUE #( refnumber = at_refnum objecttype = 'OPERATION' method = _method  objectkey  = at_oper_no ) TO it_methods.
    APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE' objectkey = at_orderid ) TO it_methods.
    APPEND VALUE #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = at_orderid ) TO it_methods.

    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / AOENNING
    IF order_type = 'ZPM5'.
      CLEAR it_methods[].
      CONCATENATE at_orderid wa_notifheader_export-notif_no INTO DATA(at_notif_no).
      APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'CREATETONOTIF'  objectkey = at_notif_no ) TO it_methods.
      APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE'        objectkey = at_orderid  ) TO it_methods.
      APPEND VALUE #( refnumber = at_refnum objecttype = ''          method = 'SAVE'           objectkey = at_notif_no ) TO it_methods.

      wa_header-orderid = at_orderid.
      wa_header-notif_type = 'Z9'.

      APPEND VALUE #( orderid  = at_orderid
                      notif_no = abap_true )
                TO it_header_up.

    ENDIF.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim



    SELECT SINGLE bukrs
    INTO lv_empresa
    FROM j_1bbranch
    WHERE branch EQ planplant.


*    wa_header-orderid       = lv_orderid.
    wa_header-funct_loc     = funct_loc.
    wa_header-order_type    = order_type.
    wa_header-short_text    = short_text.
    wa_header-loc_comp_code = lv_empresa.
    wa_header-planplant     = planplant.
    wa_header-bus_area      = bus_area.
    wa_header-mn_wk_ctr     = mn_wk_ctr.
    wa_header-plant         = plant.
    wa_header-maintplant    = maintplant.
    wa_header-loc_bus_area  = loc_bus_area.
    wa_header-plangroup     = plangroup.
    wa_header-equipment     = equipment.
    wa_header-costcenter    = costcenter.
    wa_header-start_date    = sy-datum.
    wa_header-priority      = priority.
    APPEND wa_header TO it_header.
    CLEAR wa_header.

    wa_operation-activity    = activity.
    wa_operation-control_key = control_key.
    wa_operation-description = description.
    APPEND wa_operation TO it_operation.
    CLEAR wa_operation.

    FREE: it_return.
    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods   = it_methods
        it_header    = it_header
        it_header_up = it_header_up
        it_operation = it_operation
        return       = it_return.

    CLEAR: wa_return.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 2 SECONDS.

    READ TABLE it_return INTO wa_return WITH KEY number = '112'.

    "14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / aoenning
    IF wa_return-message_v2 IS INITIAL.
      READ TABLE it_return INTO wa_return WITH KEY number = '126'.
    ENDIF.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim

    me->at_numero_ordem = wa_return-message_v2.

*   Adicionando zero a esquerda.
    me->at_numero_ordem = |{ me->at_numero_ordem ALPHA = IN }|.

    CLEAR: me->at_numero_nota.
    me->at_numero_nota = wa_notifheader_export-notif_no.



    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / aoenning
    IF create_notif EQ abap_true.
      "Preenche o numero da ordem na operação
      DATA: lt_oprol3 TYPE TABLE OF bapi_alm_olist_relation,
            lt_return TYPE TABLE OF bapiret2.

      FREE: it_return.
      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
        EXPORTING
          number   = me->at_numero_ordem
        TABLES
          et_oprol = lt_oprol3
          return   = lt_return.

      FREE: it_nota.
      it_nota = VALUE #( ( aufnr = me->at_numero_ordem
                         qmnum = wa_notifheader_export-notif_no ) ).

      CALL FUNCTION 'ZPM_ATUALIZA_OBJETOS_ORDEM' IN BACKGROUND TASK AS SEPARATE UNIT
        EXPORTING
          i_aufnr = me->at_numero_ordem
          i_notas = it_nota "wa_notifheader_export-notif_no
        TABLES
          i_oprol = lt_oprol3.

    ENDIF.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim


*   Verificar local instalação da ordem.
    SELECT SINGLE *
    FROM iloa AS a
    INNER JOIN afih AS b ON b~iloan EQ a~iloan
    INTO CORRESPONDING FIELDS OF ls_iloa
     WHERE b~aufnr EQ at_numero_ordem.


    IF ls_iloa-tplnr IS NOT INITIAL AND ls_iloa-tplnr NE funct_loc.

      "FF - 11/04/2024 #137726 - inicio
      IF wa_notifheader_export-notif_no IS NOT INITIAL.
        PERFORM shdb_modif_notif USING wa_notifheader_export-notif_no.
      ENDIF.
      "FF - 11/04/2024 #137726 - fim


      PERFORM shdb_mod_ordem USING funct_loc me->at_numero_ordem.

      IF wa_notifheader_export-notif_no IS NOT INITIAL.
        me->z_modify_nota_emprestimo(
          EXPORTING
            equipment = equipment
            planplant = planplant
            tplnr     = funct_loc
            notif_no  = wa_notifheader_export-notif_no
        ).
      ENDIF.
    ENDIF.


*    CLEAR: local.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Criar ordem de manutanção devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_ORDER_MAINTAIN

**********************************************************************************
*& Descrição: Encerrar ordens de manutenção do equipamento                      &*
*& Atributo.: WA_DATA_GENERAL, WA_RETURN                                        &*
*& Parâmetro: EQUIPMENT                                                         &*
**********************************************************************************
  METHOD z_encerrar_todas_ordens.

    CLEAR: it_methods, wa_return.
    DATA: lv_refnum     TYPE ifrefnum,
          vg_aufnr      TYPE aufnr,
          ws_data_ordem TYPE viaufkst.

*     DATA:  EQUIPMENT  TYPE EQUNR,
*             STANDORDER TYPE DAUFN,
*             SETTLORDER TYPE ILOM_ORDST.

    IF standorder IS NOT INITIAL.
      CLEAR: wa_methods, wa_header, wa_return.
      FREE: it_methods, it_header,it_return, t_return.

      wa_methods-refnumber = 1.
      wa_methods-objecttype = 'HEADER'.
      wa_methods-method = 'TECHNICALCOMPLETE '.
      wa_methods-objectkey = standorder.
      APPEND wa_methods TO it_methods.

      wa_methods-objecttype = ''.
      wa_methods-method = 'SAVE'.
      APPEND wa_methods TO it_methods.

      wa_header-orderid = standorder.
      APPEND wa_header TO it_header.

      FREE: it_return.
      CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
        TABLES
          it_methods = it_methods
          it_header  = it_header
          return     = it_return.

      APPEND LINES OF it_return TO t_return.
      IF NOT line_exists( it_return[ type = 'E' ] ).

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = wa_return.
      ENDIF.
    ENDIF.

    IF settlorder IS NOT INITIAL.
      CLEAR: wa_methods, wa_header, wa_return.
      FREE: it_methods, it_header,it_return, t_return.

      wa_methods-refnumber = 1.
      wa_methods-objecttype = 'HEADER'.
      wa_methods-method = 'TECHNICALCOMPLETE '.
      wa_methods-objectkey = settlorder.
      APPEND wa_methods TO it_methods.

      wa_methods-objecttype = ''.
      wa_methods-method = 'SAVE'.
      APPEND wa_methods TO it_methods.

      wa_header-orderid = settlorder.
      APPEND wa_header TO it_header.

      FREE: it_return.
      CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
        TABLES
          it_methods = it_methods
          it_header  = it_header
          return     = it_return.

      APPEND LINES OF it_return TO t_return.
      IF NOT line_exists( it_return[ type = 'E' ] ).
        CLEAR: wa_return.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = wa_return.
        WAIT UP TO 2 SECONDS.
      ENDIF.
    ENDIF.

    "Verificar se tem nota vinculada e encerrar.
    IF it_methods IS NOT INITIAL.
      LOOP AT it_methods ASSIGNING FIELD-SYMBOL(<ws_method>).

        CLEAR: ws_data_ordem.
        IF <ws_method>-objectkey IS NOT INITIAL.
          vg_aufnr = <ws_method>-objectkey.
          z_get_data_ordem(
            EXPORTING
              aufnr         = CONV #( vg_aufnr )
            IMPORTING
              ws_data_ordem = ws_data_ordem
          ).

          IF ws_data_ordem-qmnum IS NOT INITIAL.
            z_close_nota_emprestimo( notif_no = ws_data_ordem-qmnum
                                     refdate  = sy-datum
                                     reftime  = sy-uzeit ).
          ENDIF.
        ENDIF.
        CLEAR:  ws_data_ordem, vg_aufnr.
      ENDLOOP.
    ENDIF.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'encerrar ordem '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_ENCERRAR_TODAS_ORDENS

**********************************************************************************
*& Descrição: Verificar equipamento superior                                    &*
*& Atributo.: WA_EQUZ                                                           &*
*& Parâmetro: EQUIPMENT                                                         &*
**********************************************************************************
  METHOD z_equi_superior.
    CLEAR: wa_retorn.

    SELECT SINGLE *
    FROM equz AS a
    INNER JOIN equi AS b ON b~equnr EQ a~equnr
    INTO CORRESPONDING FIELDS OF w_equz
    WHERE a~equnr EQ equnr
      AND a~iwerk EQ iwerk
      AND a~datbi EQ '99991231'
      AND b~eqtyp IN ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A

    IF w_equz-hequi IS NOT INITIAL.
      wa_retorn = abap_true.
    ENDIF.
    CLEAR w_equz.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Verificar equipamento superior'
                  filial_origem   = iwerk
                  filial_destino  = iwerk
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.


**********************************************************************************
*& Descrição: Encerrar ordens de manutenção do equipamento                      &*
*& Atributo.: WA_DATA_GENERAL, WA_RETURN                                        &*
*& Parâmetro: EQUIPMENT                                                         &*
**********************************************************************************
  METHOD z_encerrar_todas_ordens_dev.

    CLEAR: it_methods, wa_return.

    DATA: lv_refnum  TYPE ifrefnum.

    DEFINE add_wa_methods.
      wa_methods-refnumber  = &1.
      wa_methods-objecttype = &2.
      wa_methods-method     = &3.
      wa_methods-objectkey  = &4.
      APPEND wa_methods TO it_methods.
      CLEAR wa_methods.
    END-OF-DEFINITION.

    lv_refnum  = 1.

    SHIFT lv_refnum RIGHT DELETING TRAILING space.
    TRANSLATE lv_refnum USING '0'.

    IF standorder     IS NOT INITIAL.
      add_wa_methods:
       lv_refnum 'HEADER'  'TECHNICALCOMPLETE' standorder.
    ENDIF.

    IF settlorder IS NOT INITIAL.
      add_wa_methods:
       lv_refnum 'HEADER'  'TECHNICALCOMPLETE' settlorder.
    ENDIF.

    add_wa_methods:
     lv_refnum       ''               'SAVE' '1'.

    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods = it_methods.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
  ENDMETHOD.                    "Z_ENCERRAR_TODAS_ORDENS

**********************************************************************************
*& Descrição: Recebe número da ordem criada                                     &*
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Retorno..: NUMERO_ORDEM                                                      &*
**********************************************************************************
  METHOD z_criar_ordens_manutenc_get.
    numero_ordem = me->at_numero_ordem.
  ENDMETHOD.                    "Z_CRIAR_ORDENS_MANUTENC_GET


**********************************************************************************
*& Descrição: Recebe número da ordem criada                                     &*
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Retorno..: NUMERO_ORDEM                                                      &*
**********************************************************************************
  METHOD z_criar_ordens_manutenc_get_dv.
    CLEAR numero_ordem.
    numero_ordem = me->at_numero_ordem.
  ENDMETHOD.                    "Z_CRIAR_ORDENS_MANUTENC_GET

**********************************************************************************
*& Descrição: Exibir status do equipamento                                      &*
*& Parâmetro: EQUIPMENT, IT_SYSTEM_STATUS, IT_USER_STATUS                       &*
*& Atributos Globais                                                            &*
**********************************************************************************
  METHOD z_status_equipamento.

    CLEAR: wa_return, it_system_status,
           it_user_status.

    CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
      EXPORTING
        equipment     = equipment
        language      = sy-langu
      IMPORTING
        return        = wa_return
      TABLES
        system_status = it_system_status
        user_status   = it_user_status.
  ENDMETHOD.                    "Z_STATUS_EQUIPAMENTO

**********************************************************************************
*& Descrição: Exibir detalhes do equipamento                                    &*
*& Parâmetro: EQUIPMENT,                                                        &*
*& Atributos: WA_DATA_GENERAL, WA_RETUR                                         &*
**********************************************************************************

  METHOD z_detalhes_equipamento.

    CLEAR: wa_data_general, wa_return
         , at_data_general-standorder
         , at_data_general-settlorder.

    FREE: it_zpmt0079.

    CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
      EXPORTING
        equipment        = equipment
      IMPORTING
        data_general_exp = wa_data_general
        return           = wa_return.

    at_costcenter_origem       = wa_data_general-costcenter.
    at_data_general-standorder = wa_data_general-standorder.
    at_data_general-settlorder = wa_data_general-settlorder.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = equipment
                  tp_processo     = 'Recuperar dados equipamento'
                  filial_origem   = wa_data_general-costcenter
                  filial_destino  = ''
                  kostl_origem    = equipment
                  kostl_destino   = equipment
                  tcode           = sy-tcode
                  bezei           = equipment
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim

  ENDMETHOD.                    "Z_MODIFICAR_EQUIPAMENTO

**********************************************************************************
*& Descrição: Desinstalar equipamento do local atual                            &*
*& Parâmetro: EQUIPMENT,                                                        &*
*& Atributos: WA_RETURN_BAPI_EQMT                                               &*
**********************************************************************************
  METHOD z_desinstal_equipamento.
    DATA: ws_veiculo   TYPE ztpm_m_veic_mobile.

    CLEAR: wa_return_bapi_eqmt, wa_return.
    DATA: local   TYPE iflot,
          lv_line TYPE bsvx-sttxt.

    SELECT SINGLE a~mandt e~tplnr e~pltxt a~equnr a~erdat a~aedat b~eqtyp b~eqart b~objnr a~iwerk a~datbi a~hequi c~eqktx
    FROM equz AS a
    INNER JOIN equi AS b ON b~equnr EQ a~equnr
    INNER JOIN eqkt AS c ON c~equnr EQ b~equnr
    INNER JOIN iloa AS d ON d~iloan EQ a~iloan
    INNER JOIN iflotx AS e ON e~tplnr EQ d~tplnr
    INTO CORRESPONDING FIELDS OF ws_veiculo
      WHERE a~equnr EQ equipment
       AND  a~datbi EQ '99991231'
       AND  c~spras EQ sy-langu.

    IF ws_veiculo-tplnr IS INITIAL.
      ws_veiculo-tplnr = funcloc.
    ENDIF.

    CLEAR: me->zlocal_origem.
    me->zlocal_origem = ws_veiculo-tplnr.

*** Inicio - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN
    IF ws_veiculo-objnr IS NOT INITIAL.

      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          client           = sy-mandt
          objnr            = ws_veiculo-objnr
          spras            = sy-langu
        IMPORTING
          line             = lv_line
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc = 0.
        IF lv_line EQ 'INAT' OR lv_line EQ 'MREL'.
          ws_veiculo-istat = '0'.
        ELSE.
          ws_veiculo-istat = '1'.
        ENDIF.
      ENDIF.

    ENDIF.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN

    CALL FUNCTION 'BAPI_EQMT_DISMANTLEFL'
      EXPORTING
        equipment = equipment
        funcloc   = ws_veiculo-tplnr
        date      = sy-datlo
        time      = sy-timlo
      IMPORTING
        return    = wa_return_bapi_eqmt.

*-US 158036-13-12-2024-#158036-RJF-Início
    IF wa_return_bapi_eqmt-type EQ 'E'.
      gw_erro = CORRESPONDING #( wa_return_bapi_eqmt ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = wa_return.
    ELSE.
*-US 158036-13-12-2024-#158036-RJF-Fim

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = wa_return.
      WAIT UP TO 02 SECONDS.

*-US 158036-13-12-2024-#158036-RJF-Início
    ENDIF.
*-US 158036-13-12-2024-#158036-RJF-Fim


*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Desinstalar equipamento do local atual '
                  filial_origem   = ws_veiculo-tplnr
                  filial_destino  = ws_veiculo-tplnr
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_EQUI_DISMANTLE


**********************************************************************************
*& Descrição: Seleciona local de transferencia                           &*                                                                              *
*& Atributo.: local                                                   &*
*& Parâmetro: werks                           &*
**********************************************************************************
  METHOD z_seleciona_local_tranf.
    DATA: ws_iflo TYPE iflo,
          zfltyp  TYPE iflo-fltyp.
    "Seleciona o local de transferencia.
    CLEAR: local, zfltyp, ws_iflo.
    zfltyp = 'Y'.
    SELECT SINGLE *
    FROM iflo INTO ws_iflo
    WHERE iwerk EQ werks
      AND fltyp EQ zfltyp.
    IF sy-subrc EQ 0.
      local = ws_iflo-tplnr.
    ENDIF.

  ENDMETHOD.                    "Z_LIMPAR_TELA

**********************************************************************************
*& Descrição: Instalar equipamento no centro destino                            &*                                                                              *
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Parâmetro: EQUIPMENT, FUNCLOC, WA_RETURN_BAPI_EQMT                           &*
**********************************************************************************
  METHOD z_instalar_equipamento.

    CLEAR: wa_return_bapi_eqmt, wa_return.

    CALL FUNCTION 'BAPI_EQMT_INSTALLFL'
      EXPORTING
        equipment = equipment
        funcloc   = tplnr
        date      = sy-datlo
        time      = sy-timlo
      IMPORTING
        return    = wa_return_bapi_eqmt.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 2 SECONDS.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Instalar equipamento '
                  filial_origem   = tplnr
                  filial_destino  = tplnr
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_EQUI_INSTALLFL

**********************************************************************************
*& Descrição: Instalar equipamento no centro destino                            &*                                                                              *
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Parâmetro: EQUIPMENT, FUNCLOC, WA_RETURN_BAPI_EQMT                           &*
**********************************************************************************
  METHOD z_instalar_equipamento_dev.
    CLEAR: wa_return_bapi_eqmt.


    CALL FUNCTION 'BAPI_EQMT_INSTALLFL'
      EXPORTING
        equipment = equipment
        funcloc   = tplnr
        date      = sy-datlo
        time      = sy-timlo
      IMPORTING
        return    = wa_return_bapi_eqmt.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 2 SECONDS.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Instala equipamento devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_EQUI_INSTALLFL

**********************************************************************************
*& Descrição: Modificar equipamento com centro destino                          &*
*& Atributo.: WA_DATA_GENERAL, WA_DATA_GENERALX, WA_RETURN,                     &*
*&            WA_DATA_SPECIFIC, WA_DATA_SPECIFICX                               &*
*& Parâmetro: MAINTPLANT, PLANPLANT, BUS_AREA, COSTCENTER, WORK_CTR,            &*
*&            STANDORDER, SETTLORDER                                            &*
**********************************************************************************
  METHOD z_modificar_equipamento.

    CLEAR: wa_data_general, wa_data_generalx, wa_data_specific,
           wa_data_specificx, wa_return.
    DATA: lv_empresa TYPE bukrs.

    "Buscar empresa

    SELECT SINGLE bukrs
        INTO lv_empresa
        FROM j_1bbranch
        WHERE branch EQ planplant.

    "FF - 10/04/2024 #137726 - inicio
    wa_data_general-asset_no   = imobilizado.
    wa_data_general-sub_number = subimobilizado.
    wa_data_general-plangroup  = 'FRO'.
    wa_data_general-authgrp    = '0001'. "De 0006 para 0001
    "FF - 10/04/2024 #137726 - fim

    wa_data_general-maintplant = maintplant.
    wa_data_general-bus_area   = bus_area  .
    wa_data_general-costcenter = costcenter.
    wa_data_general-comp_code  = lv_empresa.
    wa_data_general-planplant  = planplant.
    wa_data_general-work_ctr   = work_ctr.
    wa_data_general-standorder = standorder.
    wa_data_general-settlorder = settlorder.

    "FF - 10/04/2024 #137726 - inicio
    IF wa_data_general-asset_no IS NOT INITIAL.
      wa_data_generalx-asset_no   = 'X'.
    ENDIF.

    IF wa_data_general-sub_number IS NOT INITIAL.
      wa_data_generalx-sub_number = 'X'.
    ENDIF.

    wa_data_generalx-plangroup  = 'X'.
    wa_data_generalx-authgrp    = 'X'.
    "FF - 10/04/2024 #137726 - fim

    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-maintplant = 'X'.
    wa_data_generalx-bus_area   = 'X'.
    wa_data_generalx-costcenter = 'X'.
    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-work_ctr   = 'X'.

    IF wa_data_general-standorder IS NOT INITIAL.
      wa_data_generalx-standorder = 'X'.
    ENDIF.

    IF wa_data_general-settlorder IS NOT INITIAL.
      wa_data_generalx-settlorder = 'X'.
    ENDIF.

    CALL FUNCTION 'BAPI_EQUI_CHANGE'
      EXPORTING
        equipment      = equipment
        data_general   = wa_data_general
        data_generalx  = wa_data_generalx
        data_specific  = wa_data_specific
        data_specificx = wa_data_specificx
        valid_date     = sy-datum
        valid_time     = sy-uzeit
      IMPORTING
        return         = wa_return.
*        RETURN         = IT_RETURN.

*-US 158036-13-12-2024-#158036-RJF-Inicio
    IF wa_return-type EQ 'E'.
      gw_erro = wa_return.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = wa_return.
    ELSE.
*-US 158036-13-12-2024-#158036-RJF-Fim

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = wa_return.
      WAIT UP TO 02 SECONDS.

*-US 158036-13-12-2024-#158036-RJF-Inicio
    ENDIF.
*-US 158036-13-12-2024-#158036-RJF-Fim

*    CALL FUNCTION 'ENQUE_SLEEP'
*      EXPORTING
*        SECONDS = 10.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Modificar equipamento '
                  filial_origem   = planplant
                  filial_destino  = planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = costcenter
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_EQUI_CHANGE


**********************************************************************************
*& Descrição: Modificar equipamento com centro destino                          &*
*& Atributo.: WA_DATA_GENERAL, WA_DATA_GENERALX, WA_RETURN,                     &*
*&            WA_DATA_SPECIFIC, WA_DATA_SPECIFICX                               &*
*& Parâmetro: MAINTPLANT, PLANPLANT, BUS_AREA, COSTCENTER, WORK_CTR,            &*
*&            STANDORDER, SETTLORDER                                            &*
**********************************************************************************
  METHOD z_modificar_equipamento_dev.
    DATA: lv_empresa TYPE bukrs.

    CLEAR: wa_data_general, wa_data_generalx, wa_data_specific,
           wa_data_specificx, wa_return,lv_empresa.

    SELECT SINGLE bukrs
         INTO lv_empresa
         FROM j_1bbranch
         WHERE branch EQ planplant.

    "FF - 10/04/2024 #137726 - inicio
    wa_data_general-asset_no   = imobilizado.
    wa_data_general-sub_number = subimobilizado.
    wa_data_general-plangroup  = 'FRO'.
    wa_data_general-authgrp    = '0006'. "De 0001 para 0006
    "FF - 10/04/2024 #137726 - fim

    wa_data_general-maintplant = maintplant.
    wa_data_general-bus_area   = bus_area  .
    wa_data_general-costcenter = costcenter.
    wa_data_general-comp_code  = lv_empresa.
    wa_data_general-planplant  = planplant.
    wa_data_general-work_ctr   = work_ctr.

    wa_data_general-standorder = standorder.
    wa_data_general-settlorder = settlorder.

    "FF - 10/04/2024 #137726 - inicio
    IF wa_data_general-asset_no IS NOT INITIAL.
      wa_data_generalx-asset_no   = 'X'.
    ENDIF.

    IF wa_data_general-sub_number IS NOT INITIAL.
      wa_data_generalx-sub_number = 'X'.
    ENDIF.

    wa_data_generalx-plangroup  = 'X'.
    wa_data_generalx-authgrp    = 'X'.
    "FF - 10/04/2024 #137726 - fim

    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-maintplant = 'X'.
    wa_data_generalx-bus_area   = 'X'.
    wa_data_generalx-costcenter = 'X'.
    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-work_ctr   = 'X'.

    IF wa_data_general-standorder IS NOT INITIAL.
      wa_data_generalx-standorder = 'X'.
    ENDIF.

    IF wa_data_general-settlorder IS NOT INITIAL.
      wa_data_generalx-settlorder = 'X'.
    ENDIF.

*    WA_DATA_SPECIFIC-READ_FLOC  = |{ PLANPLANT }.{ 'FRO' }|.

    CALL FUNCTION 'BAPI_EQUI_CHANGE'
      EXPORTING
        equipment      = equipment
        data_general   = wa_data_general
        data_generalx  = wa_data_generalx
        data_specific  = wa_data_specific
        data_specificx = wa_data_specificx
        valid_date     = sy-datum
        valid_time     = sy-uzeit
      IMPORTING
        return         = wa_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 2 SECONDS.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Modifica equipamento devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                    "Z_EQUI_CHANGE

**********************************************************************************
*& Descrição: Selecionar os planos referêntes ao equipamento                    &*                                                                 &*
*& Parâmetro: EQUIPMENT                                                         &*
*& Atributos Globais                                                            &*
**********************************************************************************
  METHOD z_selecionar_planos.

    DATA: it_equnr TYPE RANGE OF equi-equnr,
          it_warpl TYPE RANGE OF vimpla-warpl,
          it_mptyp TYPE RANGE OF vimpla-mptyp,
          it_strat TYPE RANGE OF vimpla-strat,
          it_tplnr TYPE RANGE OF vimpla-tplnr,
          it_kdauf TYPE RANGE OF vimpla-kdauf,
          it_kdpos TYPE RANGE OF vimpla-kdpos,
          it_prot  TYPE RANGE OF sprot_u,
          wa_equnr LIKE LINE OF it_equnr.

    CLEAR: it_equnr,it_warpl,it_mptyp,it_strat,it_tplnr,
           it_kdauf,it_kdpos,it_prot.

*   Função "MAINTENANCE_PLAN_SELECTION" retorna os planos do eqpto.

    wa_equnr-low    = equipment.
    wa_equnr-sign   = 'I' .
    wa_equnr-option = 'EQ'.
    APPEND wa_equnr TO it_equnr.
    CLEAR wa_equnr.

    CALL FUNCTION 'MAINTENANCE_PLAN_SELECTION'
      TABLES
        i_warpl = it_warpl
        i_mptyp = it_mptyp
        i_strat = it_strat
        i_equnr = it_equnr
        i_tplnr = it_tplnr
        i_kdauf = it_kdauf
        i_kdpos = it_kdpos
        i_selc  = it_selc_plan
        i_prot  = it_prot.

  ENDMETHOD.                    "Z_PLAN_SELECTION

**********************************************************************************
*& Descrição: Selecionar os planos referêntes ao equipamento                    &*                                                                 &*
*& Parâmetro: EQUIPMENT                                                         &*
*& Atributos Globais                                                            &*
**********************************************************************************
  METHOD z_selecionar_planos_dev.

    DATA: it_equnr TYPE RANGE OF equi-equnr,
          it_warpl TYPE RANGE OF vimpla-warpl,
          it_mptyp TYPE RANGE OF vimpla-mptyp,
          it_strat TYPE RANGE OF vimpla-strat,
          it_tplnr TYPE RANGE OF vimpla-tplnr,
          it_kdauf TYPE RANGE OF vimpla-kdauf,
          it_kdpos TYPE RANGE OF vimpla-kdpos,
          it_prot  TYPE RANGE OF sprot_u,
          wa_equnr LIKE LINE OF it_equnr.

    CLEAR: it_equnr,it_warpl,it_mptyp,it_strat,it_tplnr,
           it_kdauf,it_kdpos,it_prot.

*   Função "MAINTENANCE_PLAN_SELECTION" retorna os planos do eqpto.

    wa_equnr-low    = equipment.
    wa_equnr-sign   = 'I' .
    wa_equnr-option = 'EQ'.
    APPEND wa_equnr TO it_equnr.
    CLEAR wa_equnr.

    CALL FUNCTION 'MAINTENANCE_PLAN_SELECTION'
      TABLES
        i_warpl = it_warpl
        i_mptyp = it_mptyp
        i_strat = it_strat
        i_equnr = it_equnr
        i_tplnr = it_tplnr
        i_kdauf = it_kdauf
        i_kdpos = it_kdpos
        i_selc  = it_selc_plan
        i_prot  = it_prot.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Selecionar planos devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


    IF it_zpmt0079 IS NOT INITIAL.
      MODIFY zpmt0079 FROM TABLE it_zpmt0079.
      COMMIT WORK.
    ENDIF.
*GGARAUJO1 - 12/09/2024 - IR190024 - fim

  ENDMETHOD.                    "Z_PLAN_SELECTION

**********************************************************************************
*& Descrição: Capturar ID do equipamento. Ex: (OFICINA)                         &*
*& Parâmetro: WA_MPOS - Declarado no programa                                   &*
*& Atributos Globais                                                            &*
**********************************************************************************
  METHOD z_set_id_equipament.
    CLEAR wa_mpos.

    SELECT SINGLE *
      FROM mpos
      INTO wa_mpos
     WHERE iwerk = swerk
       AND equnr = equnr
       AND gewrk <> ''.

    id_equipment = wa_mpos-gewrk.

  ENDMETHOD.                    "Z_SET_ID_EQUIPAMENT

*& Descrição: Capturar ID do equipamento. Ex: (OFICINA)                         &*
*& Parâmetro: WA_MPOS - Declarado no programa                                   &*
*& Atributos Globais                                                            &*
**********************************************************************************
  METHOD z_set_id_equipament_dev.
    CLEAR wa_mpos.

    SELECT SINGLE *
      FROM mpos
      INTO wa_mpos
     WHERE iwerk = swerk
       AND gewrk <> ''.

    id_equipment = wa_mpos-gewrk.

  ENDMETHOD.                    "Z_SET_ID_EQUIPAMENT

**********************************************************************************
*& Descrição: Modificar todos os planos referente ao equipamento                &*
*& Atributo.: WA_IMPOS, IT_IMHIS, IT_IMPLA, IT_IMMPT, - Decl. no programa       &*
*&            WA_RETURN                                                         &*
*& Parâmetro: EQUIPMENT, IWERK, GEWRK, GSBER, LAUFN                             &*                                         *
**********************************************************************************
  METHOD z_modificar_planos.

    CLEAR: it_impos, it_imhis,
           it_immpt, it_impla,
           it_impos[], it_imhis[],
           it_immpt[], it_impla[].

*     Percorre todos os planos do equipamento

    LOOP AT it_selc_plan INTO wa_selc_plan.

*     Capturar as informações do plano do equipamento.

      SELECT SINGLE *
        FROM mpos
        INTO wa_impos
       WHERE warpl = wa_selc_plan-warpl.

*     Capturar o numerador de grupos referente ao centro destino.

      SELECT SINGLE *
        FROM plko
        INTO wa_plko
       WHERE plnnr = wa_impos-plnnr
         AND werks = tbx_centro_destino
         AND plnty = 'A'.

*      z_seleciona_local_tranf(
*        EXPORTING
*          werks = swerk
*        IMPORTING
*          local = DATA(_tplnr)
*      ).

*      DATA(_tplnr) = |{ swerk }.{ 'FRO' }|.
*      IF _tplnr IS NOT INITIAL.
      SELECT SINGLE *
      FROM iloa
      INTO @DATA(_iloa)
        WHERE tplnr EQ @tplnr.
*      ENDIF.

      IF  _iloa IS NOT INITIAL.
        DATA(_iloan) = _iloa-iloan.
      ENDIF.

*     Modifica o plano conforme informações do destino.

      wa_impos-equnr = equipment.
      wa_impos-aedat = sy-datum.
      wa_impos-aenam = sy-uname.
      wa_impos-plnty = wa_plko-plnty.
      wa_impos-plnnr = wa_plko-plnnr.
      wa_impos-plnal = wa_plko-plnal.
      wa_impos-iwerk = swerk.
      wa_impos-gewrk = gewrk.
      wa_impos-gsber = gsber.
      wa_impos-iloan = _iloan.
      wa_impos-tplnr = loc_instalacao. "FF - 22.11.2023 - ins
*      WA_IMPOS-LAUFN = LAUFN.
      APPEND wa_impos TO it_impos.

*FF - 22.11.2023 - inicio
      UPDATE iloa SET swerk = swerk
                      gsber = gsber
                      kostl = at_costcenter_destino
                      WHERE iloan = wa_impos-iloan.

      COMMIT WORK.
*FF - 22.11.2023 - fim


    ENDLOOP.

    CALL FUNCTION 'MAINTENANCE_PLAN_POST'
      EXPORTING
        x_xaktyp = 'V'
      TABLES
        imhis    = it_imhis
        immpt    = it_immpt
        impla    = it_impla
        impos    = it_impos.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.

    WAIT UP TO 02 SECONDS.

    CLEAR: _iloan,  _iloa.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Modifica plano '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                "Z_MODIFICAR_PLANOS



**********************************************************************************
*& Descrição: Modificar todos os planos referente ao equipamento                &*
*& Atributo.: WA_IMPOS, IT_IMHIS, IT_IMPLA, IT_IMMPT, - Decl. no programa       &*
*&            WA_RETURN                                                         &*
*& Parâmetro: EQUIPMENT, IWERK, GEWRK, GSBER, LAUFN                             &*                                         *
**********************************************************************************
  METHOD z_modificar_planos_dev.

    CLEAR: it_impos, it_imhis,
           it_immpt, it_impla.

*     Percorre todos os planos do equipamento

    LOOP AT it_selc_plan INTO wa_selc_plan.

*     Capturar as informações do plano do equipamento.

      SELECT SINGLE *
        FROM mpos
        INTO wa_impos
       WHERE warpl = wa_selc_plan-warpl.

*     Capturar o numerador de grupos referente ao centro destino.

      SELECT SINGLE *
        FROM plko
        INTO wa_plko
       WHERE plnnr EQ wa_impos-bautl
         AND werks = swerk
         AND plnty = 'A'.

      z_seleciona_local_tranf(
        EXPORTING
          werks = swerk
        IMPORTING
          local = DATA(_tplnr)
      ).

      IF _tplnr IS INITIAL.
        CONCATENATE swerk '.FRO' INTO _tplnr.
      ENDIF.

      IF _tplnr IS NOT INITIAL.
        SELECT SINGLE *
        FROM iloa
        INTO @DATA(_iloa)
          WHERE tplnr EQ @_tplnr.
      ENDIF.

      IF  _iloa IS NOT INITIAL.
        DATA(_iloan) = _iloa-iloan.
      ENDIF.

*     Modifica o plano conforme informações do destino.

      wa_impos-equnr = equipment.
      wa_impos-aedat = sy-datum.
      wa_impos-aenam = sy-uname.
      wa_impos-plnty = wa_plko-plnty.
      wa_impos-plnnr = wa_plko-plnnr.
      wa_impos-plnal = wa_plko-plnal.
      wa_impos-iwerk = swerk.
      wa_impos-gewrk = gewrk.
      wa_impos-gsber = gsber.
      wa_impos-iloan = _iloan.
      wa_impos-tplnr = loc_instalacao. "FF - 22.11.2023 - ins
*      WA_IMPOS-LAUFN = LAUFN.
      APPEND wa_impos TO it_impos.

*FF - 22.11.2023 - inicio
      UPDATE iloa SET swerk = swerk
                      gsber = gsber
                      kostl = at_costcenter_destino
                      WHERE iloan = wa_impos-iloan.

      COMMIT WORK.
*FF - 22.11.2023 - fim

    ENDLOOP.

    CALL FUNCTION 'MAINTENANCE_PLAN_POST'
      EXPORTING
        x_xaktyp = 'V'
      TABLES
        imhis    = it_imhis
        immpt    = it_immpt
        impla    = it_impla
        impos    = it_impos.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      IMPORTING
        return = wa_return.
    WAIT UP TO 2 SECONDS.

    CLEAR: _tplnr, _iloan,  _iloa.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'modificar planos devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


    IF it_zpmt0079 IS NOT INITIAL.
      MODIFY zpmt0079 FROM TABLE it_zpmt0079.
      COMMIT WORK.
    ENDIF.
*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.                "Z_MODIFICAR_PLANOS

  METHOD z_busca_imobilizado.
**********************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
**********************************************************************************************

*-US 142094-08-10-2024-#142094-RJF-Inicio
*    SELECT SINGLE EQUNR,  ANLNR, ANLUN
*    INTO @DATA(LS_ITOB)
*    FROM ITOB
*    WHERE EQUNR = @EQUIPAMENTO
*      AND IWERK = @IWERK.
*
*    IF SY-SUBRC IS INITIAL.
*      IMOBILIZADO = LS_ITOB-ANLNR.
*    ELSE.
*-US 142094-08-10-2024-#142094-RJF-fim
    CLEAR imobilizado .
    SELECT SINGLE a~*
      FROM fleet AS a
      INNER JOIN equi AS b ON b~objnr EQ a~objnr
      INTO  @DATA(wa_fleet)
      WHERE b~equnr =  @equipamento.

    imobilizado = wa_fleet-zzimobilizado.
    "SUBIMOBILIZADO = "NÃO VAI TER
*-US 142094-08-10-2024-#142094-RJF-Inicio
*    ENDIF.
*-US 142094-08-10-2024-#142094-RJF-fim
**********************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - FIM
**********************************************************************************************
*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    APPEND VALUE #( equnr = tbx_equipamento
                  tp_processo     = 'Busca imobilizado '
                  filial_origem   = iwerk
                  filial_destino  = iwerk
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) TO it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  ENDMETHOD.

ENDCLASS.                    "ZBAPIS IMPLEMENTATION


*&-----------------------------------------------------------------------------*
*& CLASS LCL_EVENT_DEFINITION                                                  *
*& AUTOR: ENIO JESUS                                                           *
*& 13.07.2015                                                                  *
*&-----------------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

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

*&-------------------------------------------------------------------*
*&  CLASS LCL_EVENT_HANDLER IMPLEMENTATION                           *
*&  AUTOR: ENIO JESUS                                                *
*&  13.07.2015                                                       *
*&-------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    CHECK e_row-rowtype IS INITIAL.

*    CASE g_ts_0100-subscreen.
*      WHEN '0110'.
*        PERFORM sel_dados_equip USING e_row e_column-fieldname.
*
*      WHEN '0120'.
*        PERFORM sel_dados_equip_emprest USING e_row e_column-fieldname.
*
*      WHEN '0130'.
*        PERFORM sel_dados_equip_resposav USING e_row e_column-fieldname.
*      WHEN OTHERS.
*    ENDCASE.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

**********************************************************************
*& Descrição: Registrar evento de click no checkbox                 &*
*& Parâmetro: ER_DATA_CHANGED->                                     &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD on_data_changed.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      CASE ls_good-fieldname.

        WHEN 'CBX_ORD_ABAST'.
          READ TABLE it_saida_emprestimo_equi INTO wa_saida_emprestimo_equi
               INDEX ls_good-row_id.

          wa_saida_emprestimo_equi-cbx_ord_abast = ls_good-value.
          MODIFY it_saida_emprestimo_equi FROM wa_saida_emprestimo_equi INDEX ls_good-row_id.

        WHEN 'CBX_DEVOLVER'.
          READ TABLE it_saida_equi_responsavel INTO wa_saida_equi_responsavel
               INDEX ls_good-row_id.

          wa_saida_equi_responsavel-cbx_devolver = ls_good-value.
          MODIFY it_saida_equi_responsavel FROM wa_saida_equi_responsavel INDEX ls_good-row_id.

        WHEN 'DT_DEVOLUCAO'.
          READ TABLE it_saida_equi_responsavel INTO wa_saida_equi_responsavel
               INDEX ls_good-row_id.

          wa_saida_equi_responsavel-dt_devolucao = ls_good-value.
          MODIFY it_saida_equi_responsavel FROM wa_saida_equi_responsavel INDEX ls_good-row_id.

          CALL METHOD obj_alv_0130->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        WHEN 'HR_DEVOLUCAO'.
          READ TABLE it_saida_equi_responsavel INTO wa_saida_equi_responsavel
               INDEX ls_good-row_id.

          wa_saida_equi_responsavel-hr_devolucao = ls_good-value.
          MODIFY it_saida_equi_responsavel FROM wa_saida_equi_responsavel INDEX ls_good-row_id.

          CALL METHOD obj_alv_0130->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
*** Inicio - Rubenilson - 24.12.24 - US138088
        WHEN 'DEVOLUCAO_AUTOMATICA'.
          READ TABLE it_saida_emprestimo_equi INTO wa_saida_emprestimo_equi
               INDEX ls_good-row_id.

          wa_saida_emprestimo_equi-devolucao_automatica = ls_good-value.
          MODIFY it_saida_emprestimo_equi FROM wa_saida_emprestimo_equi INDEX ls_good-row_id.
*** Fim - Rubenilson - 24.12.24 - US138088
      ENDCASE.
    ENDLOOP.

    CLEAR: wa_saida_equi_responsavel, wa_saida_equi_disponiveis, wa_saida_equi_emprestados,
           ls_good, er_data_changed->mt_good_cells.
  ENDMETHOD.                    "ON_DATA_CHANGED_CHECKBOX

  METHOD on_data_changed_finished.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
      IMPORTING
        e_messagem = wa_mensagem
      TABLES
        it_msgs    = it_msg_return.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

**********************************************************************
*& Descrição: Criar botões em ALV                                   &*
*& Parâmetro: E_OBJECT->                                            &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD set_toolbar.

    CLEAR: wa_toolbar, e_object->mt_toolbar.
    CASE g_ts_0100-pressed_tab.

*    Botão <Emprestar equipamentos> Tela 0100;
      WHEN 'TAB_DISPONIVEIS'.
        wa_toolbar-butn_type = 3.
        APPEND wa_toolbar TO e_object->mt_toolbar.

        CLEAR wa_toolbar.
        wa_toolbar-function     = 'BTN_EMPRESTAR_EQUI'.
        wa_toolbar-icon         =  icon_delivery.
        wa_toolbar-quickinfo    = 'Emprestar Equipamentos'.
        wa_toolbar-butn_type    = 0.
        wa_toolbar-text         = 'Emprestar Equipamentos'.
        APPEND wa_toolbar TO e_object->mt_toolbar.

        CLEAR wa_toolbar.
        wa_toolbar-function     = '&MB_FILTER'.
        wa_toolbar-icon         =  icon_filter.
        wa_toolbar-quickinfo    = 'Filtrar Equipamentos'.
        wa_toolbar-butn_type    = 0.
        wa_toolbar-text         = 'Filtrar Equipamentos'.
        APPEND wa_toolbar TO e_object->mt_toolbar.

      WHEN 'TAB_RESPONSAVEL'.
        wa_toolbar-butn_type = 3.
        APPEND wa_toolbar TO e_object->mt_toolbar.
        CLEAR wa_toolbar.

        wa_toolbar-function     = 'BTN_DEVOLVER_EQUI'.
        wa_toolbar-icon         =  icon_transportation_mode.
        wa_toolbar-quickinfo    = 'Devolver Equipamentos'.
        wa_toolbar-butn_type    = 0.
        wa_toolbar-text         = 'Devolver Equipamentos'.
        APPEND wa_toolbar TO e_object->mt_toolbar.

        CLEAR wa_toolbar.
        wa_toolbar-function     = '&MB_FILTER'.
        wa_toolbar-icon         =  icon_filter.
        wa_toolbar-quickinfo    = 'Filtrar Equipamentos'.
        wa_toolbar-butn_type    = 0.
        wa_toolbar-text         = 'Filtrar Equipamentos'.
        APPEND wa_toolbar TO e_object->mt_toolbar.
      WHEN OTHERS.
    ENDCASE.

    IF ( sy-dynnr = '0300' ).
      CLEAR: wa_toolbar, e_object->mt_toolbar.

      wa_toolbar-function     = 'BTN_SELECIONAR'.
      wa_toolbar-icon         = icon_system_mark.
      wa_toolbar-butn_type    = 0.
      wa_toolbar-text         = 'Selecionar'.
      APPEND wa_toolbar TO e_object->mt_toolbar.

      wa_toolbar-butn_type = 3.
      APPEND wa_toolbar TO e_object->mt_toolbar.
      CLEAR wa_toolbar.

      wa_toolbar-function     = 'BTN_DESMONTAR'.
      wa_toolbar-icon         = icon_convert_all.
      wa_toolbar-butn_type    = 0.
      wa_toolbar-text         = 'Desmontar'.
      APPEND wa_toolbar TO e_object->mt_toolbar.

      wa_toolbar-butn_type = 3.
      APPEND wa_toolbar TO e_object->mt_toolbar.
      CLEAR wa_toolbar.

      wa_toolbar-function     = 'BTN_ABOUT'.
      wa_toolbar-icon         = icon_information.
      wa_toolbar-butn_type    = 0.
      wa_toolbar-text         = 'Sobre'.
      APPEND wa_toolbar TO e_object->mt_toolbar.

    ENDIF.

  ENDMETHOD.                    "SET_TOOLBAR

**********************************************************************
*& Descrição: Registra ação nos botões da ALV                       &*
*& Parâmetro: E_UCOMM                                               &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD get_ucomm.

    DATA: r_checar_info_emprestimo  TYPE REF TO zuteis,
          r_checar_info_devolucao   TYPE REF TO zuteis,
          r_iniciar_processo_zbapis TYPE REF TO zbapis.

    CREATE OBJECT: r_checar_info_emprestimo,
                   r_iniciar_processo_zbapis.

    CASE e_ucomm.

*    Registra ação no botão 'Emprestar Equipamentos' Tela 0100.
*    RETURN_STATUS é um atributo global que retorna se a operação não foi bem sucedida.

      WHEN 'BTN_FILT_EQUI'.

        DATA: p_tname TYPE char50.
        DATA: p_fname TYPE char50.
        DATA: dfies_tab TYPE TABLE OF equi.

*        PERFORM FILT.

      WHEN 'BTN_EMPRESTAR_EQUI'.
        CLEAR: it_selected_rows, it_saida_emprestimo_equi, return_status, wa_saida_emprestimo_equi, wa_saida_equi_disponiveis.

*        IF IT_MSG_RETURN IS NOT INITIAL.
*          MESSAGE 'Corrigir erros pendentes' TYPE 'I' DISPLAY LIKE 'E'.
*
*        ENDIF.


        CALL METHOD obj_alv_0110->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        DESCRIBE TABLE it_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.

        ELSE.
*** Inicio - Rubenilson - 23.12.24 - US138088
          DATA: lt_celltab TYPE lvc_t_styl,
                wa_celltab TYPE lvc_s_styl.

          wa_celltab-fieldname = 'CBX_ORD_ABAST'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          INSERT wa_celltab INTO lt_celltab INDEX 1.

          wa_celltab-fieldname = 'CBX_ORD_REMON'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          INSERT wa_celltab INTO lt_celltab INDEX 2.

          wa_celltab-fieldname = 'DEVOLUCAO_AUTOMATICA'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_enabled.

          INSERT wa_celltab INTO lt_celltab INDEX 3.

          wa_celltab-fieldname = 'EQKTX'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          INSERT wa_celltab INTO lt_celltab INDEX 4.

          wa_celltab-fieldname = 'EQUNR'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          INSERT wa_celltab INTO lt_celltab INDEX 5.

          wa_celltab-fieldname = 'IWERK'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          INSERT wa_celltab INTO lt_celltab INDEX 6.
*** Fim - Rubenilson - 23.12.24 - US138088

          LOOP AT it_selected_rows INTO wa_selected_rows.
            READ TABLE it_saida_equi_disponiveis INTO wa_saida_equi_disponiveis INDEX wa_selected_rows-index.

            wa_saida_emprestimo_equi-equnr         = |{ wa_saida_equi_disponiveis-equnr ALPHA = OUT }|.
            wa_saida_emprestimo_equi-iwerk         = wa_saida_equi_disponiveis-iwerk.
            wa_saida_emprestimo_equi-eqktx         = wa_saida_equi_disponiveis-eqktx.
            wa_saida_emprestimo_equi-cbx_ord_remon = 'X'.

            CLEAR p_eq_sup.
            IF wa_saida_equi_disponiveis-cbx_ord_abast EQ abap_true.
              wa_saida_emprestimo_equi-cbx_ord_abast = 'X'.
              wa_saida_emprestimo_equi-eq_sup = wa_saida_equi_disponiveis-eq_sup.
            ELSE.
              wa_saida_emprestimo_equi-eq_inf = wa_saida_equi_disponiveis-eq_inf.
            ENDIF.

            wa_saida_emprestimo_equi-celltab = lt_celltab.

            APPEND wa_saida_emprestimo_equi TO it_saida_emprestimo_equi.
            CLEAR: wa_saida_emprestimo_equi, wa_saida_emprestimo_equi.
          ENDLOOP.

          r_checar_info_emprestimo->z_checar_equi_hierarchy( IMPORTING e_return = return_status ).

          IF ( it_status_equnr IS INITIAL ).
*            CALL SCREEN 0200 STARTING AT 5 5.
          ELSE.
*            CALL SCREEN 0300 STARTING AT 8 8.
          ENDIF.
        ENDIF.

      WHEN 'BTN_DEVOLVER_EQUI'.

        REFRESH: it_selected_rows.
        DATA: lt_equz TYPE equz.
        DATA: gt_equz      TYPE TABLE OF equz.
        FREE: it_saida_dev_equi, it_status_equnr, it_selected_rows, gt_equz, it_status_hequi.

        CLEAR: wa_selected_rows, wa_saida_equi_responsavel,
        wa_zequi_emprestimo, wa_saida_dev_equi, wa_status_equnr, wa_status_hequi..


        CALL METHOD obj_alv_0130->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        DESCRIBE TABLE it_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
          EXIT.
        ELSE.
          LOOP AT it_selected_rows INTO wa_selected_rows.
            READ TABLE it_saida_equi_responsavel INTO wa_saida_equi_responsavel INDEX wa_selected_rows-index.
            CLEAR wa_zequi_emprestimo.
            wa_saida_equi_responsavel-equnr = |{ wa_saida_equi_responsavel-equnr ALPHA = IN }|.
            SELECT SINGLE *
              FROM zequi_emprestimo
              INTO wa_zequi_emprestimo
             WHERE equnr = wa_saida_equi_responsavel-equnr.

            IF wa_zequi_emprestimo IS NOT INITIAL.

              MOVE-CORRESPONDING wa_zequi_emprestimo TO wa_saida_dev_equi.

              APPEND wa_saida_dev_equi TO it_saida_dev_equi.
              CLEAR: wa_saida_dev_equi, wa_saida_equi_responsavel.
            ENDIF.

*            IF WA_ZEQUI_EMPRESTIMO-EQUNR IS NOT INITIAL.
            FREE gt_equz.
            CLEAR lt_equz.

            SELECT SINGLE *
            FROM equz AS a
            INNER JOIN equi AS b ON b~equnr EQ a~equnr
            INTO CORRESPONDING FIELDS OF lt_equz
              WHERE a~equnr EQ wa_zequi_emprestimo-equnr
                AND a~datbi EQ '99991231'.
*                  AND B~EQTYP EQ 'V'.

            IF lt_equz-hequi IS NOT INITIAL.
              READ TABLE it_saida_equi_responsavel INTO DATA(eq_respos) WITH KEY equnr =  |{ lt_equz-hequi ALPHA = OUT }|.
              IF sy-subrc = 0.
                wa_status_equnr-equnr   = lt_equz-equnr."Equipamento inferior

                SELECT SINGLE *
                FROM eqkt
                INTO @DATA(s_eqkt_)
                WHERE equnr EQ @wa_status_equnr-equnr.
                wa_status_equnr-eqktx = s_eqkt_-eqktx.
                wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior

                CLEAR s_eqkt_.
                SELECT SINGLE *
                FROM eqkt
                INTO s_eqkt_
                WHERE equnr EQ wa_status_equnr-hequi.
                wa_status_equnr-eqktx_ = s_eqkt_-eqktx.
                wa_status_equnr-eqp_sup = abap_true.
                APPEND wa_status_equnr TO it_status_equnr.
                CLEAR wa_status_equnr.
              ENDIF.

              FREE gt_equz.
              SELECT *
              FROM equz AS a
              INNER JOIN equi AS b ON b~equnr EQ a~equnr
              INTO CORRESPONDING FIELDS OF TABLE gt_equz
                WHERE a~hequi EQ lt_equz-equnr
                  AND a~datbi EQ '99991231'
                  AND b~eqtyp NE 'A' "FF - 05.04.2024 - ins
                  AND b~eqtyp NE 'V'
                  AND b~eqtyp NE '1' "FF - 22.11.2023 - ins
                  AND b~eqtyp NE '2'
                  AND b~eqtyp NE '3'
                  AND b~eqtyp NE '4'.


              IF gt_equz IS NOT INITIAL.
                LOOP AT gt_equz INTO DATA(lg_equz) WHERE hequi EQ lt_equz-equnr.
                  IF sy-subrc = 0.
                    wa_status_equnr-equnr   = |{ lg_equz-equnr ALPHA = IN }|. "Equipamento inferior
                    CLEAR s_eqkt_.
                    SELECT SINGLE *
                    FROM eqkt
                    INTO s_eqkt_
                      WHERE equnr EQ lg_equz-equnr.
                    wa_status_equnr-eqktx = s_eqkt_-eqktx.

                    wa_status_equnr-hequi   = lg_equz-hequi."Equipamento Superior
                    CLEAR s_eqkt_.
                    SELECT SINGLE *
                    FROM eqkt
                    INTO s_eqkt_
                      WHERE equnr EQ  wa_status_equnr-hequi.
                    wa_status_equnr-eqktx_ = s_eqkt_-eqktx.
                    wa_status_equnr-eqp_inf = abap_true.

                    APPEND wa_status_equnr TO it_status_equnr.
                    CLEAR: wa_status_equnr, lg_equz.
                    CONTINUE.
                  ENDIF.
                ENDLOOP.
              ENDIF.

              FREE gt_equz.
              SELECT *
              FROM equz AS a
              INNER JOIN equi AS b ON b~equnr EQ a~equnr
              INTO CORRESPONDING FIELDS OF TABLE gt_equz
                WHERE a~hequi EQ lt_equz-hequi
                  AND a~datbi EQ '99991231'
                  AND b~eqtyp NE 'A' "FF - 05.04.2024 - ins
                  AND b~eqtyp NE 'V'
                  AND b~eqtyp NE '1' "FF - 22.11.2023 - ins
                  AND b~eqtyp NE '2'
                  AND b~eqtyp NE '3'
                  AND b~eqtyp NE '4'.


              IF gt_equz IS NOT INITIAL.
                LOOP AT gt_equz INTO lt_equz WHERE hequi EQ lt_equz-hequi.
                  IF sy-subrc = 0.
                    wa_status_equnr-equnr   = |{ lt_equz-equnr ALPHA = IN }|. "Equipamento inferior
                    CLEAR s_eqkt_.
                    SELECT SINGLE *
                    FROM eqkt
                    INTO s_eqkt_
                      WHERE equnr EQ lt_equz-equnr.
                    wa_status_equnr-eqktx = s_eqkt_-eqktx.

                    wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior
                    CLEAR s_eqkt_.
                    SELECT SINGLE *
                    FROM eqkt
                    INTO s_eqkt_
                      WHERE equnr EQ  wa_status_equnr-hequi.
                    wa_status_equnr-eqktx_ = s_eqkt_-eqktx.
                    wa_status_equnr-eqp_inf = abap_true.

                    APPEND wa_status_equnr TO it_status_equnr.
                    CLEAR: wa_status_equnr, lt_equz.
                    CONTINUE.
                  ENDIF.
                ENDLOOP.
              ENDIF.

            ELSE.

              FREE gt_equz.
              SELECT *
              FROM equz AS a
              INNER JOIN equi AS b ON b~equnr EQ a~equnr
              INTO CORRESPONDING FIELDS OF TABLE gt_equz
                WHERE a~hequi EQ wa_zequi_emprestimo-equnr
                  AND a~datbi EQ '99991231'.
*                  AND B~EQTYP EQ 'V'.

              IF gt_equz IS NOT INITIAL.
                LOOP AT gt_equz INTO lt_equz WHERE hequi EQ wa_zequi_emprestimo-equnr.
                  READ TABLE it_saida_equi_responsavel INTO DATA(_eq_respos) WITH KEY equnr =  |{ lt_equz-equnr ALPHA = OUT }|.
                  IF sy-subrc = 0.
                    wa_status_equnr-equnr   = |{ _eq_respos-equnr ALPHA = IN }|. "Equipamento inferior
                    SELECT SINGLE *
                    FROM eqkt
                    INTO @DATA(s_eqkt)
                      WHERE equnr EQ  @wa_status_equnr-equnr.
                    wa_status_equnr-eqktx = s_eqkt-eqktx.

                    wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior
                    CLEAR s_eqkt.
                    SELECT SINGLE *
                    FROM eqkt
                    INTO s_eqkt
                      WHERE equnr EQ  wa_status_equnr-hequi.
                    wa_status_equnr-eqktx_ = s_eqkt-eqktx.
                    wa_status_equnr-eqp_inf = abap_true.

                    APPEND wa_status_equnr TO it_status_equnr.
                    CLEAR wa_status_equnr.
                    CONTINUE.
                  ENDIF.

                  wa_status_equnr-equnr   = |{ lt_equz-equnr ALPHA = IN }|. "Equipamento inferior
                  CLEAR s_eqkt.
                  SELECT SINGLE *
                  FROM eqkt
                  INTO s_eqkt
                    WHERE equnr EQ wa_status_equnr-equnr.
                  wa_status_equnr-eqktx = s_eqkt-eqktx.
                  wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior

                  CLEAR s_eqkt.
                  SELECT SINGLE *
                  FROM eqkt
                  INTO s_eqkt
                    WHERE equnr EQ  wa_status_equnr-hequi.
                  wa_status_equnr-eqktx_ = s_eqkt-eqktx.
                  wa_status_equnr-eqp_inf = abap_true.

                  APPEND wa_status_equnr TO it_status_equnr.
                  CLEAR wa_status_equnr.
                ENDLOOP.

                FREE it_status_hequi.
                MOVE it_status_equnr TO it_status_hequi.

                LOOP AT it_status_hequi INTO wa_status_hequi WHERE eqp_inf EQ abap_true.

                  CLEAR: gt_equz, lt_equz.
                  SELECT *
                  FROM equz AS a
                  INNER JOIN equi AS b ON b~equnr EQ a~equnr
                  INTO CORRESPONDING FIELDS OF TABLE gt_equz
                    WHERE a~hequi EQ wa_status_hequi-equnr
                      AND a~datbi EQ '99991231'.
*                  AND B~EQTYP EQ 'V'.

                  LOOP AT gt_equz INTO lt_equz WHERE hequi EQ wa_status_hequi-equnr.
                    IF sy-subrc = 0.
                      wa_status_equnr-equnr   = |{ lt_equz-equnr ALPHA = IN }|. "Equipamento inferior
                      CLEAR s_eqkt.
                      SELECT SINGLE *
                      FROM eqkt
                      INTO s_eqkt
                        WHERE equnr EQ wa_status_equnr-equnr.
                      wa_status_equnr-eqktx = s_eqkt-eqktx.
                      wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior

                      CLEAR s_eqkt.
                      SELECT SINGLE *
                      FROM eqkt
                      INTO s_eqkt
                        WHERE equnr EQ  wa_status_equnr-hequi.
                      wa_status_equnr-eqktx_ = s_eqkt-eqktx.
                      wa_status_equnr-eqp_inf = abap_true.

                      APPEND wa_status_equnr TO it_status_equnr.
                      CLEAR: wa_status_equnr, lt_equz.
                    ENDIF.
                  ENDLOOP.
                ENDLOOP.
              ENDIF.
            ENDIF.
*            ENDIF.
          ENDLOOP.


          SORT it_saida_dev_equi ASCENDING BY equnr.
          SORT it_status_equnr ASCENDING BY hequi.
          LOOP AT it_saida_dev_equi INTO DATA(w_dev).
            LOOP AT it_status_equnr INTO DATA(_status) WHERE hequi = w_dev-equnr.
              IF _status-eqp_inf IS INITIAL.
                DELETE it_status_equnr INDEX sy-tabix.
                CONTINUE.
              ENDIF.
            ENDLOOP.

            LOOP AT it_status_equnr INTO DATA(status) WHERE equnr = w_dev-equnr.
              IF status-eqp_sup IS INITIAL.
                DELETE it_status_equnr INDEX sy-tabix.
                CONTINUE.
              ENDIF.
            ENDLOOP.
          ENDLOOP.

          CLEAR p_devolucao.
          IF it_status_equnr IS INITIAL.
*            CALL SCREEN 400 STARTING AT 5 5 .
          ELSE.
            p_devolucao = abap_true.
*            CALL SCREEN 300 STARTING AT 5 5 .
          ENDIF.
*
        ENDIF.
      WHEN 'BTN_SELECIONAR'.

        REFRESH: it_selected_rows.

        CLEAR: c_destino, c_origem, wa_status_equnr, wa_saida_emprestimo_equi.

*** Inicio - Rubenilson - 23.12.24 - US138088
        wa_celltab-fieldname = 'CBX_ORD_ABAST'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        INSERT wa_celltab INTO lt_celltab INDEX 1.

        wa_celltab-fieldname = 'CBX_ORD_REMON'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        INSERT wa_celltab INTO lt_celltab INDEX 2.

        wa_celltab-fieldname = 'DEVOLUCAO_AUTOMATICA'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_enabled.

        INSERT wa_celltab INTO lt_celltab INDEX 3.

        wa_celltab-fieldname = 'EQKTX'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        INSERT wa_celltab INTO lt_celltab INDEX 4.

        wa_celltab-fieldname = 'EQUNR'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        INSERT wa_celltab INTO lt_celltab INDEX 5.

        wa_celltab-fieldname = 'IWERK'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        INSERT wa_celltab INTO lt_celltab INDEX 6.
*** Fim - Rubenilson - 23.12.24 - US138088

        CALL METHOD obj_alv_status->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        DESCRIBE TABLE it_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.

          SORT it_selected_rows STABLE DESCENDING.

*-US 158036-26-11-2024-#158036-RJF-Início
          LOOP AT it_selected_rows INTO wa_selected_rows.
            READ TABLE it_status_equnr INTO wa_status_equnr INDEX wa_selected_rows-index.
            IF wa_status_equnr-status EQ icon_alert.
              DATA(lv_error) = abap_on.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF lv_error EQ abap_on.
            MESSAGE wa_status_equnr-det TYPE 'I' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
*-US 158036-26-11-2024-#158036-RJF-Fim

          LOOP AT it_selected_rows INTO wa_selected_rows.
            READ TABLE it_status_equnr INTO wa_status_equnr INDEX wa_selected_rows-index.

            "Verifica se o equipamento é superior ou inferior;
            IF p_devolucao IS INITIAL.
              READ TABLE it_saida_emprestimo_equi INTO wa_saida_emprestimo_equi WITH KEY equnr = wa_status_equnr-equnr.

              "No caso de um eqpto inferior, iremos selecionar o superior, que
              "nesse caso é WA_STATUS_EQUNR-HEQUI;

              IF ( sy-subrc IS INITIAL ).
                CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
                  EXPORTING
                    equipment         = wa_status_equnr-hequi
                  IMPORTING
                    data_specific_exp = wa_data_specific_exp
                    data_general_exp  = wa_data_general.

                wa_saida_emprestimo_equi-equnr = wa_status_equnr-hequi.

                "No caso de um eqpto superior, iremos selecionar o inferior deste,
                "que neste caso é WA_STATUS_EQUNR-EQUNR;

              ELSE.
                wa_status_equnr-equnr = |{ wa_status_equnr-equnr ALPHA = IN }|.
                CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
                  EXPORTING
                    equipment         = wa_status_equnr-equnr
                  IMPORTING
                    data_specific_exp = wa_data_specific_exp
                    data_general_exp  = wa_data_general.

                wa_saida_emprestimo_equi-equnr = wa_status_equnr-equnr.
              ENDIF.

              wa_saida_emprestimo_equi-iwerk         = wa_data_general-maintplant.
              wa_saida_emprestimo_equi-eqktx         = wa_data_general-descript.
              wa_saida_emprestimo_equi-celltab       = lt_celltab. " Rubenilson - 23.12.24 - US138088
              SELECT SINGLE *
              FROM equi
              INTO @DATA(ls_equi)
                WHERE equnr EQ @wa_saida_emprestimo_equi-equnr.

              IF  ls_equi-eqtyp = 'V' OR
                  ls_equi-eqtyp = 'A' OR "FF - 05.04.24  - ins
                  ls_equi-eqtyp = '1' OR "FF - 22.11.23  - ins
                  ls_equi-eqtyp = '2' OR
                  ls_equi-eqtyp = '3' OR
                  ls_equi-eqtyp = '4'.

                wa_saida_emprestimo_equi-cbx_ord_remon = 'X'.
              ENDIF.

              IF wa_status_equnr-eqp_sup IS INITIAL.
                wa_saida_emprestimo_equi-cbx_ord_abast = abap_false.
              ELSE.
                wa_saida_emprestimo_equi-cbx_ord_abast = abap_true.
              ENDIF.

              wa_status_equnr-equnr = |{ wa_status_equnr-equnr ALPHA = OUT }|.
              APPEND wa_saida_emprestimo_equi TO it_saida_emprestimo_equi.
              CLEAR: ls_equi, wa_saida_emprestimo_equi.

              DELETE it_status_equnr WHERE equnr = wa_status_equnr-equnr
                                       AND hequi = wa_status_equnr-hequi.


              CALL METHOD obj_alv_status->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.

              CHECK ( it_status_equnr IS INITIAL ).
              SORT it_saida_emprestimo_equi DESCENDING BY equnr.
              DELETE ADJACENT DUPLICATES FROM it_saida_emprestimo_equi COMPARING equnr.
*              LEAVE TO SCREEN 0200.


            ELSE.

              IF wa_status_equnr-eqp_inf IS NOT INITIAL.

                READ TABLE it_saida_equi_responsavel INTO wa_saida_equi_responsavel WITH KEY equnr = |{ wa_status_equnr-equnr ALPHA = OUT }|.
                IF sy-subrc = 0.
                  MOVE-CORRESPONDING wa_saida_equi_responsavel TO wa_saida_dev_equi.
                  APPEND wa_saida_dev_equi TO it_saida_dev_equi.
                  DELETE it_status_equnr WHERE equnr = |{ wa_saida_dev_equi-equnr ALPHA = IN }|.
*                                       AND HEQUI = WA_STATUS_EQUNR-HEQUI.
                  CLEAR: wa_saida_dev_equi, wa_saida_equi_responsavel.

                ELSE.

                  MOVE-CORRESPONDING wa_status_equnr  TO wa_saida_dev_equi.
                  SELECT SINGLE *
                  FROM zequi_emprestimo
                  INTO @DATA(_zequi_emprestimo)
                    WHERE equnr EQ @wa_status_equnr-hequi.

                  wa_saida_dev_equi-erdat       = sy-datum.
                  wa_saida_dev_equi-iwerk       = _zequi_emprestimo-iwerk.
                  wa_saida_dev_equi-cent_origem = _zequi_emprestimo-cent_origem.
                  APPEND wa_saida_dev_equi TO it_saida_dev_equi.
                  DELETE it_status_equnr WHERE equnr = |{ wa_saida_dev_equi-equnr ALPHA = IN }|.
*                                       AND HEQUI = WA_STATUS_EQUNR-HEQUI.
                  CLEAR: wa_saida_dev_equi, wa_saida_equi_responsavel.
                ENDIF.

              ELSEIF wa_status_equnr-eqp_sup IS NOT INITIAL..
                READ TABLE it_saida_equi_responsavel INTO wa_saida_equi_responsavel WITH KEY equnr = |{ wa_status_equnr-hequi ALPHA = OUT }|.
                IF sy-subrc = 0.
                  MOVE-CORRESPONDING wa_saida_equi_responsavel TO wa_saida_dev_equi.
                  APPEND wa_saida_dev_equi TO it_saida_dev_equi.
                  DELETE it_status_equnr WHERE hequi = |{ wa_saida_dev_equi-equnr ALPHA = IN }|.

*                                       AND HEQUI = WA_STATUS_EQUNR-HEQUI.
                  CLEAR: wa_saida_dev_equi, wa_saida_equi_responsavel.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.

          CALL METHOD obj_alv_status->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

          CHECK ( it_status_equnr IS INITIAL ).
          SORT it_saida_dev_equi DESCENDING BY equnr.


*         Adicionando por prioridade.
          DATA: cont TYPE p DECIMALS 1.
          CLEAR cont.

          LOOP AT it_saida_dev_equi ASSIGNING FIELD-SYMBOL(<dev_equi>).
            <dev_equi>-equnr = |{ <dev_equi>-equnr ALPHA = IN }|.
            CLEAR: lt_equz.
            SELECT SINGLE *
            FROM equz AS a
            INNER JOIN equi AS b ON b~equnr EQ a~equnr
            INTO CORRESPONDING FIELDS OF lt_equz
              WHERE a~equnr EQ <dev_equi>-equnr
                AND a~datbi EQ '99991231'
                AND b~eqtyp IN ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A

            ADD 1 TO cont.

            IF lt_equz-equnr IS NOT INITIAL AND lt_equz-hequi IS INITIAL .
              <dev_equi>-sequencia = 1.
              CONTINUE.
            ENDIF.

            IF lt_equz-equnr IS NOT INITIAL AND lt_equz-hequi IS NOT INITIAL.
              <dev_equi>-sequencia = 2.
              CONTINUE.
            ENDIF.

            IF lt_equz IS INITIAL.
              <dev_equi>-sequencia = cont.
            ENDIF.
          ENDLOOP.

          SORT it_saida_dev_equi ASCENDING BY sequencia.
          DELETE ADJACENT DUPLICATES FROM it_saida_dev_equi COMPARING equnr.
*          LEAVE TO SCREEN 0400.
        ENDIF.


      WHEN 'BTN_DESMONTAR'.
        REFRESH: it_selected_rows.

        CALL METHOD obj_alv_status->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        DESCRIBE TABLE it_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          SORT it_selected_rows STABLE DESCENDING.
          IF p_devolucao IS INITIAL.
            LOOP AT it_selected_rows INTO wa_selected_rows.
              READ TABLE it_status_equnr INTO wa_status_equnr INDEX wa_selected_rows-index.

              "Verifica se o equipamento é superior ou inferior;

              READ TABLE it_saida_emprestimo_equi INTO wa_saida_emprestimo_equi
                WITH KEY equnr = wa_status_equnr-hequi.

              "No caso de um eqpto inferior, iremos selecionar o superior, que
              "nesse caso é WA_STATUS_EQUNR-HEQUI;

              IF ( sy-subrc IS INITIAL ).
                CALL FUNCTION 'BAPI_EQMT_DISMANTLEHR'
                  EXPORTING
                    equipment = wa_status_equnr-equnr
                    superequi = wa_status_equnr-hequi
                    date      = sy-datlo
                    time      = sy-timlo
                  IMPORTING
                    return    = wa_return.

                "No caso de um eqpto superior, iremos selecionar o inferior deste,
                "que neste caso é WA_STATUS_EQUNR-EQUNR;

              ELSE.
                CALL FUNCTION 'BAPI_EQMT_DISMANTLEHR'
                  EXPORTING
                    equipment = wa_status_equnr-hequi
                    superequi = wa_status_equnr-equnr
                    date      = sy-datlo
                    time      = sy-timlo
                  IMPORTING
                    return    = wa_return.
              ENDIF.

              DELETE it_status_equnr WHERE equnr = wa_status_equnr-equnr
                                       AND hequi = wa_status_equnr-hequi.
            ENDLOOP.

            CALL METHOD obj_alv_status->refresh_table_display
              EXPORTING
                is_stable = wa_stable.

            CHECK ( it_status_equnr IS INITIAL ).
*            LEAVE TO SCREEN 0200.
          ELSE.

            LOOP AT it_selected_rows INTO wa_selected_rows.
              READ TABLE it_status_equnr INTO wa_status_equnr INDEX wa_selected_rows-index.

              "Verifica se o equipamento é superior ou inferior;
              READ TABLE it_saida_dev_equi INTO wa_saida_dev_equi WITH KEY equnr = wa_status_equnr-hequi.

              "No caso de um eqpto inferior, iremos selecionar o superior, que
              "nesse caso é WA_STATUS_EQUNR-HEQUI;

              IF ( sy-subrc IS INITIAL ).
                CALL FUNCTION 'BAPI_EQMT_DISMANTLEHR'
                  EXPORTING
                    equipment = wa_status_equnr-equnr
                    superequi = wa_status_equnr-hequi
                    date      = sy-datlo
                    time      = sy-timlo
                  IMPORTING
                    return    = wa_return.

                "No caso de um eqpto superior, iremos selecionar o inferior deste,
                "que neste caso é WA_STATUS_EQUNR-EQUNR;

              ELSE.
                CALL FUNCTION 'BAPI_EQMT_DISMANTLEHR'
                  EXPORTING
                    equipment = wa_status_equnr-hequi
                    superequi = wa_status_equnr-equnr
                    date      = sy-datlo
                    time      = sy-timlo
                  IMPORTING
                    return    = wa_return.
              ENDIF.

              DELETE it_status_equnr WHERE equnr = wa_status_equnr-equnr
                                       AND hequi = wa_status_equnr-hequi.
            ENDLOOP.

            CALL METHOD obj_alv_status->refresh_table_display
              EXPORTING
                is_stable = wa_stable.

            CHECK ( it_status_equnr IS INITIAL ).
*            LEAVE TO SCREEN 0400.
          ENDIF.
        ENDIF.

      WHEN 'BTN_ABOUT'.
        MESSAGE i836(sd) WITH 'É necessário selecionar ou desmontar o equipamento'
                              'inferior/superior para continuar com o empréstimo/devolução.'.

      WHEN 'BTN_CHECK_EQ_INF'.
        REFRESH: it_selected_rows.
        DATA: zt_equz TYPE equz.

        CALL METHOD obj_alv_0130->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        DESCRIBE TABLE it_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          LOOP AT it_selected_rows INTO wa_selected_rows.
            READ TABLE it_saida_equi_responsavel INTO wa_saida_equi_responsavel INDEX wa_selected_rows-index.
            CLEAR wa_zequi_emprestimo.
            SELECT SINGLE *
              FROM zequi_emprestimo
              INTO wa_zequi_emprestimo
             WHERE equnr = wa_saida_equi_responsavel-equnr.

            IF wa_zequi_emprestimo IS NOT INITIAL.
              SELECT *
              FROM equz AS a
              INNER JOIN equi AS b ON b~equnr EQ a~equnr
              INTO CORRESPONDING FIELDS OF TABLE gt_equz
                WHERE a~hequi EQ wa_saida_equi_responsavel-equnr
                  AND a~datbi EQ '99991231'
                  AND b~eqtyp IN ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 - ins e "FF - 05.04.2023 - ins
            ENDIF.
          ENDLOOP.
          DATA(sy_ucomm) = 'BTN_CHECK_EQ_INF'.
        ENDIF.


*        CALL SCREEN 0100.
*        CALL METHOD OBJ_ALV_0130->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE = WA_STABLE.



    ENDCASE.
  ENDMETHOD.                    "GET_UCOMM

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
"LCL_EVENT_HANDLER IMPLEMENTATION

MODULE input_001 INPUT.
*  CALL METHOD MAIN_INSTANCE->USER_COMMAND
*    EXPORTING
*      UCOMM          = SY-UCOMM
*    EXCEPTIONS
*      INCORRECT_DATA = 4.
ENDMODULE.
