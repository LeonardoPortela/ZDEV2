*&---------------------------------------------------------------------*
*& Report  ZHCMR_RETURN_UNIORG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zhcmr_return_uniorg.

DATA: it_hrp1000     TYPE TABLE OF hrp1000,
      it_hrp1001     TYPE TABLE OF hrp1001,
      it_hrp1001_aux TYPE TABLE OF hrp1001, " RMNI 15.05.2023 CS1090505
      ws_hrp1001     TYPE hrp1001,
      w_hrp1001      TYPE hrp1001,
      w_hrp1001_aux  TYPE hrp1001,
      ws_pa0002      TYPE pa0002,
      ws_pa0001      TYPE pa0001,
      ws_pa0465      TYPE pa0465,
      ws_pa0105      TYPE pa0105,
      w_saida        TYPE zhcmt_f_uniorg,
      t_saida        TYPE TABLE OF zhcmt_f_uniorg,
      t_saida_aux    TYPE TABLE OF zhcms_return_uniorg_new.

DATA(cl_hcm_util) = NEW zcl_hcm_util( ).

*&---------------------------------------------------------------------*
*& Busca dados uniorg
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  PERFORM fm_seleciona_uniorg.
  PERFORM fm_seleciona_kostl_orcamento.
*&---------------------------------------------------------------------*
*&      Form  FM_SELECIONA_UNIORG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_seleciona_uniorg .
  TABLES  pme04.
  DATA: contract(40)  TYPE c,
        status(2)     TYPE p,
        kind_of_error TYPE c,
        struc(5).
  FIELD-SYMBOLS <struc_content>.

  SELECT SINGLE struc FROM  t549d INTO struc
       WHERE  namen   = 'ABKRS'.

  ASSIGN (struc) TO <struc_content>.

  FREE: it_hrp1000, t_saida, t_saida_aux.

  SELECT * FROM hrp1000
  INTO TABLE it_hrp1000
    WHERE plvar EQ '01'
      AND otype EQ 'O'
      AND endda >= sy-datum
      AND langu EQ sy-langu.

  CHECK  it_hrp1000 IS NOT INITIAL.

  LOOP AT it_hrp1000 ASSIGNING FIELD-SYMBOL(<ls_hrp1000>).
* Inicio - Alteracao  - RMNI - CS1090505 - 15.05.2023
    CLEAR: it_hrp1001_aux,
           ws_hrp1001,
           w_saida. "CSB - 07.05.2024

    REFRESH it_hrp1001_aux.
*    SELECT SINGLE * FROM hrp1001
*    INTO  ws_hrp1001
*      WHERE objid EQ <ls_hrp1000>-objid
*        AND otype EQ 'O'
*        AND plvar EQ '01'
*        AND relat EQ '012'
*        AND endda >= sy-datum
*        AND sclas EQ 'S'.

    SELECT * FROM hrp1001
    INTO TABLE it_hrp1001_aux
      WHERE objid EQ <ls_hrp1000>-objid
        AND otype EQ 'O'
        AND plvar EQ '01'
        AND relat EQ '012'
*        AND endda >= sy-datum
        AND sclas EQ 'S'.

    "IF sy-subrc  EQ 0.
    DELETE it_hrp1001_aux WHERE endda < sy-datum.
    SORT it_hrp1001_aux BY  begda  DESCENDING.

    READ TABLE it_hrp1001_aux INTO ws_hrp1001 INDEX 1.
* Final  - Alteracao  - RMNI - CS1090505 - 15.05.2023
    "ENDIF.

    IF sy-subrc NE 0.

      FREE: t_saida_aux.
      t_saida_aux = VALUE #( ( orgeh = <ls_hrp1000>-objid  ) ).

      IF t_saida_aux IS NOT INITIAL.
        CALL METHOD cl_hcm_util->get_gestor_uniorg_acima
          CHANGING
            t_saida = t_saida_aux[].

        READ TABLE t_saida_aux INTO DATA(f_saida) INDEX 1.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING f_saida TO w_saida.

          "Buscar Nome gestor

          SELECT SINGLE * FROM pa0002
            INTO ws_pa0002
            WHERE pernr = f_saida-pernr_gestor
              AND endda >= sy-datum.

*          w_saida-nome_gestor  = ws_pa0002-cname.
          w_saida-nome_gestor   = |{ ws_pa0002-cname CASE = UPPER }|.

          IF f_saida-pernr_gestor IS NOT INITIAL.
* Comentado no dia 19.04.2023 - O  w_saida-pernr_gestor ja vem preenchido. CSB 110286
*            "Buscar CPF gestor
*            SELECT SINGLE pernr
*              FROM pa0465
*              INTO w_saida-pernr_gestor
*              WHERE endda >= sy-datum
*              AND   cpf_nr = f_saida-cpf_gestor.

*            IF sy-subrc IS INITIAL.
            "Buscar E-mail gestor
            SELECT SINGLE usrid_long
              INTO w_saida-email_gestor
              FROM pa0105
              WHERE pernr = f_saida-pernr_gestor
              AND  subty  = 'MAIL'
              AND  endda >= sy-datum.

            SELECT SINGLE persk
              FROM pa0001
               INTO w_saida-persk_gestor
              WHERE pernr EQ f_saida-pernr_gestor
               AND endda >= sy-datum.

            IF sy-subrc IS INITIAL.

              SELECT SINGLE ptext
                  FROM t503t
                   INTO w_saida-desc_persk_gestor
                  WHERE persk EQ w_saida-persk_gestor
                   AND sprsl EQ sy-langu.

            ENDIF.
*            ENDIF.
          ENDIF.

******* Buscar Empresa, Filial, Centro de custo e Área_folha
** NOVA REGRA 14.12.2022 - BUG - 98720 - CBRAND - Inicio
          SELECT * FROM hrp1001
             INTO TABLE @DATA(it_hrp1001)
            WHERE objid EQ @f_saida-orgeh
              AND otype EQ 'O'
              AND plvar EQ '01'
              AND relat EQ '003'
              AND endda >= @sy-datum
              AND sclas EQ 'S'.

          IF sy-subrc IS INITIAL.

            LOOP AT it_hrp1001 INTO DATA(wa_hrp1001).

              w_saida-orgeh    = f_saida-orgeh.

              SELECT SINGLE *
               FROM hrp1008
               INTO @DATA(wa_hrp1008)
               WHERE objid = @wa_hrp1001-sobid AND
                     plvar = '01' AND
                     otype = 'S' AND
                     endda >= @sy-datum.
              IF sy-subrc = 0.
                w_saida-cod_empresa  = wa_hrp1008-bukrs.
                w_saida-cod_filial   = wa_hrp1008-gsber.

                SELECT SINGLE *
                 FROM hrp1001
                 INTO @DATA(wa_hrp1001_cc)
                 WHERE objid = @wa_hrp1001-sobid AND
                       plvar = '01' AND
                       otype = 'S'  AND
                       endda >= @sy-datum AND
                       sclas = 'K' .

                w_saida-cod_ccusto   =  |{ wa_hrp1001_cc-sobid+0(10) }|.

                CLEAR: pme04.
                pme04-bukrs = w_saida-cod_empresa.
                IF w_saida-cod_filial = '9121'.
                  w_saida-cod_filial = '0121'.
                  pme04-werks = '0121'. "BUG - 99292 - CBRAND
                ELSE.
                  pme04-werks = w_saida-cod_filial.
                ENDIF.
                pme04-molga = '37'.
                pme04-tclas = 'A'.

                CLEAR contract.
                CALL FUNCTION 'HR_FEATURE_BACKFIELD'
                  EXPORTING
                    feature       = 'ABKRS'
                    struc_content = <struc_content>
                    kind_of_error = kind_of_error
                  IMPORTING
                    back          = contract
                  CHANGING
                    status        = status
                  EXCEPTIONS
                    OTHERS        = 1.

                w_saida-area_folha   = contract.

                CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
                  EXPORTING
                    werks      = w_saida-cod_filial
                  IMPORTING
                    werks_text = w_saida-nome_filial.

                CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
                  EXPORTING
                    bukrs      = w_saida-cod_empresa
                    langu      = sy-langu
                  IMPORTING
                    bukrs_text = w_saida-nome_empresa.

                SELECT SINGLE ltext
                FROM cskt
                INTO w_saida-nome_ccusto
                WHERE kostl EQ w_saida-cod_ccusto
                  AND spras EQ sy-langu
                  AND datbi >= sy-datum.

                w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.

                APPEND w_saida TO t_saida.
                CLEAR: wa_hrp1001,  wa_hrp1008, wa_hrp1001_cc.
              ENDIF.
            ENDLOOP.

            CLEAR: w_hrp1001, ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida.

          ELSE.
            w_saida-orgeh    = f_saida-orgeh. " BUG - 120484 - CBRAND - 10.08.2023
            w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.
            APPEND w_saida TO t_saida.
            CLEAR: w_saida, ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001,
                   wa_hrp1001,  wa_hrp1008, wa_hrp1001_cc.
          ENDIF.

*** BUG - 98720 - CBRAND - Inicio
*          CLEAR: ws_pa0001.
*          SELECT *
*            FROM pa0001
*            INTO TABLE @DATA(it_pa0001)
*            WHERE orgeh EQ @f_saida-orgeh
*              AND endda >= @sy-datum
*              AND abkrs <> 'BA'
*              AND plans <> '99999999'.
*
*          IF sy-subrc IS INITIAL.
*            LOOP AT it_pa0001 INTO ws_pa0001.
*
*              w_saida-orgeh        = ws_pa0001-orgeh.
*              w_saida-cod_empresa  = ws_pa0001-bukrs.
*              w_saida-cod_filial   = ws_pa0001-werks.
*              w_saida-cod_ccusto   = ws_pa0001-kostl.
*              w_saida-area_folha   = ws_pa0001-abkrs.
*
*              CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
*                EXPORTING
*                  werks      = ws_pa0001-werks
*                IMPORTING
*                  werks_text = w_saida-nome_filial.
*
*              CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
*                EXPORTING
*                  bukrs      = ws_pa0001-bukrs
*                  langu      = sy-langu
*                IMPORTING
*                  bukrs_text = w_saida-nome_empresa.
*
*              SELECT SINGLE ltext
*              FROM cskt
*              INTO w_saida-nome_ccusto
*              WHERE kostl EQ ws_pa0001-kostl
*                AND spras EQ sy-langu
*                AND datbi >= sy-datum.
*
*              w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.
*
*              APPEND w_saida TO t_saida.
*              CLEAR: ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001.
*
*            ENDLOOP.
*          ELSE.
*
*            w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.
*
*            APPEND w_saida TO t_saida.
*            CLEAR: w_saida, ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001.
*
*          ENDIF.
*** BUG - 98720 - CBRAND - Fim
        ENDIF.
      ENDIF.

**===================================================================================================
    ELSE.

* Inicio - Alteracao  - RMNI - CS1090505 - 17.05.2023
      LOOP AT it_hrp1001_aux INTO w_hrp1001_aux.
        ws_hrp1001-sobid = w_hrp1001_aux-sobid.
* Final  - Alteracao  - RMNI - CS1090505 - 17.05.2023
        "Buscar Pernr gestor.
        SELECT SINGLE * FROM hrp1001
         INTO w_hrp1001
         WHERE otype EQ 'S'
           AND objid EQ ws_hrp1001-sobid
           AND plvar EQ '01'
           AND relat EQ '008'
           AND endda >= sy-datum
           AND sclas EQ sy-langu.


        IF sy-subrc NE 0.
          CONTINUE.
        ELSE.
          EXIT.
        ENDIF.
* Inicio - Alteracao  - RMNI - CS1090505 - 17.05.2023
      ENDLOOP.

      "IF sy-subrc NE 0.
      IF w_hrp1001 IS INITIAL.
* Final  - Alteracao  - RMNI - CS1090505 - 17.05.2023

        FREE: t_saida_aux.
        t_saida_aux = VALUE #( ( orgeh = <ls_hrp1000>-objid  ) ).

        CALL METHOD cl_hcm_util->get_gestor_uniorg_acima
          CHANGING
            t_saida = t_saida_aux[].

        CLEAR: f_saida.
        READ TABLE t_saida_aux INTO f_saida INDEX 1.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING f_saida TO w_saida.

          "Buscar Nome gestor
          SELECT SINGLE * FROM pa0002
            INTO ws_pa0002
            WHERE pernr = f_saida-pernr_gestor
              AND endda >= sy-datum.

*          w_saida-nome_gestor  = ws_pa0002-cname.
          w_saida-nome_gestor   = |{ ws_pa0002-cname CASE = UPPER }|.

          IF f_saida-pernr_gestor IS NOT INITIAL.

* Comentado no dia 19.04.2023 - O  w_saida-pernr_gestor ja vem preenchido. CSB #110286
*            "Buscar CPF gestor
*            SELECT SINGLE pernr
*              FROM pa0465
*              INTO w_saida-pernr_gestor
*              WHERE endda >= sy-datum
*              AND   cpf_nr = f_saida-cpf_gestor.

*            IF sy-subrc IS INITIAL.
            "Buscar E-mail gestor
            SELECT SINGLE usrid_long
              INTO w_saida-email_gestor
              FROM pa0105
              WHERE pernr = f_saida-pernr_gestor
              AND  subty  = 'MAIL'
              AND  endda >= sy-datum.

            SELECT SINGLE persk
              FROM pa0001
               INTO w_saida-persk_gestor
              WHERE pernr EQ f_saida-pernr_gestor
               AND endda >= sy-datum.

            IF sy-subrc IS INITIAL.

              SELECT SINGLE ptext
                  FROM t503t
                   INTO w_saida-desc_persk_gestor
                  WHERE persk EQ w_saida-persk_gestor
                   AND sprsl EQ sy-langu.
            ENDIF.
*           endif.
          ENDIF.

******* Buscar Empresa, Filial, Centro de custo e Área_folha
** NOVA REGRA 14.12.2022 - BUG - 98720 - CBRAND - Inicio
          CLEAR: it_hrp1001.
          SELECT * FROM hrp1001
             INTO TABLE it_hrp1001
            WHERE objid EQ <ls_hrp1000>-objid
              AND otype EQ 'O'
              AND plvar EQ '01'
              AND relat EQ '003'
              AND endda >= sy-datum
              AND sclas EQ 'S'.

          IF sy-subrc IS INITIAL.

            LOOP AT it_hrp1001 INTO wa_hrp1001.
              w_saida-orgeh    = <ls_hrp1000>-objid.

              SELECT SINGLE *
               FROM hrp1008
               INTO wa_hrp1008
               WHERE objid = wa_hrp1001-sobid AND
                     plvar = '01' AND
                     otype = 'S' AND
                     endda >= sy-datum.

              IF sy-subrc = 0.
                w_saida-cod_empresa  = wa_hrp1008-bukrs.
                w_saida-cod_filial   = wa_hrp1008-gsber.

                SELECT SINGLE *
                 FROM hrp1001
                 INTO wa_hrp1001_cc
                 WHERE objid = wa_hrp1001-sobid AND
                       plvar = '01' AND
                       otype = 'S'  AND
                       endda >= sy-datum AND
                       sclas = 'K' .

                w_saida-cod_ccusto   =  |{ wa_hrp1001_cc-sobid+0(10) }|.

                CLEAR: pme04.
                pme04-bukrs = w_saida-cod_empresa.
                IF w_saida-cod_filial = '9121'.
                  w_saida-cod_filial = '0121'.
                  pme04-werks = '0121'. "BUG - 99292 - CBRAND
                ELSE.
                  pme04-werks = w_saida-cod_filial.
                ENDIF.
                pme04-molga = '37'.
                pme04-tclas = 'A'.

                CLEAR contract.
                CALL FUNCTION 'HR_FEATURE_BACKFIELD'
                  EXPORTING
                    feature       = 'ABKRS'
                    struc_content = <struc_content>
                    kind_of_error = kind_of_error
                  IMPORTING
                    back          = contract
                  CHANGING
                    status        = status
                  EXCEPTIONS
                    OTHERS        = 1.

                w_saida-area_folha   = contract.


                CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
                  EXPORTING
                    werks      = w_saida-cod_filial
                  IMPORTING
                    werks_text = w_saida-nome_filial.

                CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
                  EXPORTING
                    bukrs      = w_saida-cod_empresa
                    langu      = sy-langu
                  IMPORTING
                    bukrs_text = w_saida-nome_empresa.

                SELECT SINGLE ltext
                FROM cskt
                INTO w_saida-nome_ccusto
                WHERE kostl EQ w_saida-cod_ccusto
                  AND spras EQ sy-langu
                  AND datbi >= sy-datum.

                w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.

                APPEND w_saida TO t_saida.
                CLEAR: wa_hrp1001, wa_hrp1008, wa_hrp1001_cc.
              ENDIF.
            ENDLOOP.

            CLEAR:  w_hrp1001, ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida.

          ELSE.
            w_saida-orgeh    = <ls_hrp1000>-objid. " BUG - 120484 - CBRAND - 10.08.2023 -
            w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.
            APPEND w_saida TO t_saida.
            CLEAR: w_saida, ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001,
                   wa_hrp1001,  wa_hrp1008, wa_hrp1001_cc.
          ENDIF.

*** BUG - 98720 - CBRAND - Inicio
*          CLEAR: ws_pa0001.
*          SELECT *
*            FROM pa0001
*            INTO TABLE @DATA(it_pa0001)
*            WHERE orgeh  EQ @f_saida-orgeh
*            AND endda >= @sy-datum
*            AND abkrs <> 'BA'
*            AND plans <> '99999999'.
*
*          IF sy-subrc IS INITIAL.
*
*            LOOP AT it_pa0001 INTO ws_pa0001.
*
*              w_saida-orgeh        = ws_pa0001-orgeh.
*              w_saida-cod_empresa  = ws_pa0001-bukrs.
*              w_saida-cod_filial   = ws_pa0001-werks.
*              w_saida-cod_ccusto   = ws_pa0001-kostl.
*              w_saida-area_folha   = ws_pa0001-abkrs.
*
*              CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
*                EXPORTING
*                  werks      = ws_pa0001-werks
*                IMPORTING
*                  werks_text = w_saida-nome_filial.
*
*              CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
*                EXPORTING
*                  bukrs      = ws_pa0001-bukrs
*                  langu      = sy-langu
*                IMPORTING
*                  bukrs_text = w_saida-nome_empresa.
*
*              SELECT SINGLE ltext
*                  FROM cskt
*                    INTO w_saida-nome_ccusto
*                   WHERE kostl EQ ws_pa0001-kostl
*                     AND spras EQ sy-langu
*                     AND datbi >= sy-datum.
*
*              w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.
*
*              APPEND w_saida TO t_saida.
*              CLEAR: ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001.
*
*            ENDLOOP.
*
*          ELSE.
*
*            w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.
*
*            APPEND w_saida TO t_saida.
*            CLEAR: w_saida, ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001.
*
*          ENDIF.
*** BUG - 98720 - CBRAND - Fim
        ENDIF.

      ELSE.

**============================================================================================================
        "Buscar Nome gestor
        IF w_hrp1001 IS  NOT INITIAL.
          CLEAR: ws_pa0002.
          SELECT SINGLE * FROM pa0002
            INTO ws_pa0002
            WHERE pernr = w_hrp1001-sobid
              AND endda >= sy-datum.


          w_saida-pernr_gestor  = w_hrp1001-sobid.
          w_saida-nome_gestor   = |{ ws_pa0002-cname CASE = UPPER }|.

          " IF sy-subrc IS INITIAL.
          "Buscar CPF gestor
          SELECT SINGLE * FROM pa0465  INTO ws_pa0465
          WHERE pernr EQ w_hrp1001-sobid
           AND endda >= sy-datum
           AND subty EQ '0001'.

          w_saida-cpf_gestor  = ws_pa0465-cpf_nr.

          "Buscar E-mail gestor
          SELECT SINGLE * FROM pa0105  INTO ws_pa0105
          WHERE pernr EQ w_hrp1001-sobid
           AND endda >= sy-datum
           AND subty = 'MAIL'.

          w_saida-email_gestor  = ws_pa0105-usrid_long.

          SELECT SINGLE persk
            FROM pa0001
             INTO w_saida-persk_gestor
            WHERE pernr EQ w_saida-pernr_gestor
             AND endda >= sy-datum.

          IF sy-subrc IS INITIAL.

            SELECT SINGLE ptext
                FROM t503t
                 INTO w_saida-desc_persk_gestor
                WHERE persk EQ w_saida-persk_gestor
                  AND sprsl EQ sy-langu.

          ENDIF.

        ENDIF.

** nova regra 14.12.2022 - bug - 98720 - cbrand - inicio
        CLEAR: it_hrp1001.
        SELECT * FROM hrp1001
           INTO TABLE it_hrp1001
          WHERE objid EQ <ls_hrp1000>-objid
            AND otype EQ 'O'
            AND plvar EQ '01'
            AND relat EQ '003'
            AND endda >= sy-datum
            AND sclas EQ 'S'.

        IF sy-subrc IS INITIAL.

          LOOP AT it_hrp1001 INTO wa_hrp1001.

            w_saida-orgeh    = <ls_hrp1000>-objid.

            SELECT SINGLE *
             FROM hrp1008
             INTO wa_hrp1008
             WHERE objid = wa_hrp1001-sobid AND
                   plvar = '01' AND
                   otype = 'S' AND
                   endda >= sy-datum.

            IF sy-subrc = 0.

              w_saida-cod_empresa  = wa_hrp1008-bukrs.
              w_saida-cod_filial   = wa_hrp1008-gsber.

              SELECT SINGLE *
               FROM hrp1001
               INTO wa_hrp1001_cc
               WHERE objid = wa_hrp1001-sobid AND
                     plvar = '01' AND
                     otype = 'S'  AND
                     endda >= sy-datum AND
                     sclas = 'K' .

              w_saida-cod_ccusto   =  |{ wa_hrp1001_cc-sobid+0(10) }|.

              CLEAR: pme04.
              pme04-bukrs = w_saida-cod_empresa.
              IF w_saida-cod_filial = '9121'.
                w_saida-cod_filial = '0121'.
                pme04-werks = '0121'.
              ELSE.
                pme04-werks = w_saida-cod_filial. "BUG - 99292 - CBRAND
              ENDIF.
              pme04-molga = '37'.
              pme04-tclas = 'A'.

              CLEAR contract.
              CALL FUNCTION 'HR_FEATURE_BACKFIELD'
                EXPORTING
                  feature       = 'ABKRS'
                  struc_content = <struc_content>
                  kind_of_error = kind_of_error
                IMPORTING
                  back          = contract
                CHANGING
                  status        = status
                EXCEPTIONS
                  OTHERS        = 1.

              w_saida-area_folha   = contract.

              CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
                EXPORTING
                  werks      = w_saida-cod_filial
                IMPORTING
                  werks_text = w_saida-nome_filial.

              CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
                EXPORTING
                  bukrs      = w_saida-cod_empresa
                  langu      = sy-langu
                IMPORTING
                  bukrs_text = w_saida-nome_empresa.

              SELECT SINGLE ltext
              FROM cskt
              INTO w_saida-nome_ccusto
              WHERE kostl EQ w_saida-cod_ccusto
                AND spras EQ sy-langu
                AND datbi >= sy-datum.

              w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.

              APPEND w_saida TO t_saida.
              CLEAR: wa_hrp1001,  wa_hrp1008, wa_hrp1001_cc.
            ENDIF.
          ENDLOOP.

          CLEAR:  ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001.

        ELSE.
          w_saida-orgeh    = <ls_hrp1000>-objid. " BUG - 120484 - CBRAND - 10.08.2023
          w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.
          APPEND w_saida TO t_saida.
          CLEAR: w_saida, ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001,
                 wa_hrp1001,  wa_hrp1008, wa_hrp1001_cc.
        ENDIF.
*** BUG - 98720 - CBRAND - Inicio
*        "Buscar Empresa, Filial, Centro de custo e Área_folha
*        CLEAR: ws_pa0001.
*        SELECT *
*          FROM pa0001
*          INTO TABLE it_pa0001
*          WHERE orgeh  EQ <ls_hrp1000>-objid
*          AND endda >= sy-datum
*          AND abkrs <> 'BA'
*          AND plans <> '99999999'.
*
*        IF sy-subrc IS INITIAL.
*          LOOP AT it_pa0001 INTO ws_pa0001.
*
*            w_saida-orgeh       = ws_pa0001-orgeh.
*            w_saida-cod_empresa = ws_pa0001-bukrs.
*            w_saida-cod_filial  = ws_pa0001-werks.
*            w_saida-cod_ccusto  = ws_pa0001-kostl.
*            w_saida-area_folha  = ws_pa0001-abkrs.
*
*            CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
*              EXPORTING
*                werks      = ws_pa0001-werks
*              IMPORTING
*                werks_text = w_saida-nome_filial.
*
*            CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
*              EXPORTING
*                bukrs      = ws_pa0001-bukrs
*                langu      = sy-langu
*              IMPORTING
*                bukrs_text = w_saida-nome_empresa.
*
*            SELECT SINGLE ltext
*                FROM cskt
*              INTO w_saida-nome_ccusto
*                WHERE kostl EQ ws_pa0001-kostl
*                  AND spras EQ sy-langu
*                  AND datbi >= sy-datum.
*
*            w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.
*
*            APPEND w_saida TO t_saida.
*            CLEAR: ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001.
*
*          ENDLOOP.
*
*        ELSE.
*
*          w_saida-stext = |{ <ls_hrp1000>-stext CASE = UPPER }|.
*
*          APPEND w_saida TO t_saida.
*          CLEAR: w_saida, ws_pa0001, ws_pa0105, ws_pa0465, ws_pa0002, f_saida, w_hrp1001.
*
*        ENDIF.
*** BUG - 98720 - CBRAND - Fim

      ENDIF.
    ENDIF.

  ENDLOOP.

  DELETE FROM zhcmt_f_uniorg.
  COMMIT WORK.

  IF t_saida IS NOT INITIAL.
    MODIFY zhcmt_f_uniorg FROM TABLE t_saida.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SELECIONA_KOSTL_ORCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_seleciona_kostl_orcamento .

  DATA: lt_idx_structure    TYPE STANDARD TABLE OF bapiacpstru,
        lt_object           TYPE STANDARD TABLE OF bapipcpobj,
        lt_tot_value        TYPE STANDARD TABLE OF bapipcptot,
        lt_per_value        TYPE STANDARD TABLE OF bapipcpval,
        lt_contrl           TYPE STANDARD TABLE OF bapipcpctrl,
        lt_return           TYPE STANDARD TABLE OF bapiret2,
        lt_zhcmt_f_orcament TYPE STANDARD TABLE OF zhcmt_f_orcament,
        wa_zhcmt_f_orcament TYPE zhcmt_f_orcament.

  DATA: ls_pcpobj  TYPE bapipcpobj,
        ls_ionra   TYPE ionra,
        ls_pcpline TYPE pcpvalline_i.

  DATA: l_kstar TYPE coep-kstar,
        l_kokrs TYPE csks-kokrs VALUE 'MAGI'.

  DATA: ls_header_info   TYPE bapiplnhdr.
  DATA: cont TYPE n LENGTH 2.
  DATA: mes TYPE n LENGTH 2.
  DATA: ano TYPE n LENGTH 4.

  DATA: l_kostl TYPE csks-kostl,
        l_saknr TYPE ska1-saknr VALUE '421101'.

********** Fim Custo Plaejado **********

  ano = sy-datum(4).

  SELECT kostl
   FROM csks
    INTO TABLE @DATA(lt_kostl)
   WHERE kokrs EQ @l_kokrs
     AND datbi >= @sy-datum.

  CHECK sy-subrc IS INITIAL.

  SORT lt_kostl BY kostl.
  DELETE ADJACENT DUPLICATES FROM lt_kostl COMPARING kostl.

  ls_header_info-co_area       = l_kokrs.
  ls_header_info-fisc_year     = ano.
  ls_header_info-period_from   = '01'.
  ls_header_info-period_to     = '12'.
  ls_header_info-version       = '0'.
  ls_header_info-plan_currtype = 'o'. "Moeda do objeto

  l_saknr = |{ l_saknr ALPHA = IN }|.

  APPEND VALUE #(
          object_index = '000001'
          value_index = '000001'
                ) TO lt_idx_structure.

  LOOP AT lt_kostl INTO DATA(wa_kostl).

    wa_kostl-kostl = |{ wa_kostl-kostl ALPHA = IN }|.

    APPEND VALUE #(
            value_index = '000001'
            cost_elem   = l_saknr
                  ) TO lt_per_value.

    APPEND VALUE #(
            object_index = '000001'
            costcenter  = wa_kostl-kostl
                   ) TO lt_object.

    CALL FUNCTION 'BAPI_PRIM_COST_READ'
      EXPORTING
        header_info   = ls_header_info
      TABLES
        idx_structure = lt_idx_structure
        object        = lt_object
        per_value     = lt_per_value
        tot_value     = lt_tot_value
        contrl        = lt_contrl
        return        = lt_return.

    READ TABLE lt_per_value INTO DATA(ls_per_value_new) INDEX 1.
    IF sy-subrc IS INITIAL.

      MOVE-CORRESPONDING ls_per_value_new TO wa_zhcmt_f_orcament.

      wa_zhcmt_f_orcament-cod_ccusto    = wa_kostl-kostl.
      wa_zhcmt_f_orcament-data_inclusao = sy-datum.
      wa_zhcmt_f_orcament-hora_inclusao = sy-uzeit.
      wa_zhcmt_f_orcament-user_inclusao = sy-uname.

    ENDIF.

********** Fim Custo Plaejado **********

********** Inicio Custo Realizado **********

    ls_pcpobj-costcenter = |{  wa_kostl-kostl ALPHA = IN }|.
    l_saknr = |{ l_saknr ALPHA = IN }|.

    CALL FUNCTION 'MAP2I_BAPIPCPOBJ_TO_PCPVALLINE'
      EXPORTING
        bapipcpobj   = ls_pcpobj
      CHANGING
        pcpvalline_i = ls_pcpline.

    MOVE-CORRESPONDING ls_pcpline TO ls_ionra.
    MOVE               l_kokrs    TO ls_ionra-kokrs.

    CALL FUNCTION 'K_OBJNR_GET_FROM_IONRA'
      CHANGING
        cs_ionra     = ls_ionra
      EXCEPTIONS
        not_possible = 1
        OTHERS       = 2.

    SELECT kokrs, wogbtr, perio
      INTO TABLE @DATA(lt_coep)
      FROM coep
      WHERE kokrs = @l_kokrs
      AND   perio BETWEEN '01' AND '12'
      AND   lednr = '00'
      AND   objnr = @ls_ionra-objnr
      AND   gjahr = @ano
      AND   wrttp = '04'
      AND   versn = '000'
      AND   kstar = @l_saknr.

    LOOP AT lt_coep INTO DATA(ls_coep).
      CASE ls_coep-perio.
        WHEN '001'.  wa_zhcmt_f_orcament-fix_val_rea01 = wa_zhcmt_f_orcament-fix_val_rea01 + ls_coep-wogbtr.
        WHEN '002'.  wa_zhcmt_f_orcament-fix_val_rea02 = wa_zhcmt_f_orcament-fix_val_rea02 + ls_coep-wogbtr.
        WHEN '003'.  wa_zhcmt_f_orcament-fix_val_rea03 = wa_zhcmt_f_orcament-fix_val_rea03 + ls_coep-wogbtr.
        WHEN '004'.  wa_zhcmt_f_orcament-fix_val_rea04 = wa_zhcmt_f_orcament-fix_val_rea04 + ls_coep-wogbtr.
        WHEN '005'.  wa_zhcmt_f_orcament-fix_val_rea05 = wa_zhcmt_f_orcament-fix_val_rea05 + ls_coep-wogbtr.
        WHEN '006'.  wa_zhcmt_f_orcament-fix_val_rea06 = wa_zhcmt_f_orcament-fix_val_rea06 + ls_coep-wogbtr.
        WHEN '007'.  wa_zhcmt_f_orcament-fix_val_rea07 = wa_zhcmt_f_orcament-fix_val_rea07 + ls_coep-wogbtr.
        WHEN '008'.  wa_zhcmt_f_orcament-fix_val_rea08 = wa_zhcmt_f_orcament-fix_val_rea08 + ls_coep-wogbtr.
        WHEN '009'.  wa_zhcmt_f_orcament-fix_val_rea09 = wa_zhcmt_f_orcament-fix_val_rea09 + ls_coep-wogbtr.
        WHEN '010'.  wa_zhcmt_f_orcament-fix_val_rea10 = wa_zhcmt_f_orcament-fix_val_rea10 + ls_coep-wogbtr.
        WHEN '011'.  wa_zhcmt_f_orcament-fix_val_rea11 = wa_zhcmt_f_orcament-fix_val_rea11 + ls_coep-wogbtr.
        WHEN '012'.  wa_zhcmt_f_orcament-fix_val_rea12 = wa_zhcmt_f_orcament-fix_val_rea12 + ls_coep-wogbtr.
      ENDCASE.
    ENDLOOP.


********** Fim Custo Realizado **********
    APPEND wa_zhcmt_f_orcament TO lt_zhcmt_f_orcament.

    FREE: lt_per_value, lt_object, lt_return, ls_pcpobj, ls_pcpline, ls_ionra, lt_coep, wa_zhcmt_f_orcament.

  ENDLOOP.

  CHECK lt_zhcmt_f_orcament IS NOT INITIAL.

  DELETE FROM zhcmt_f_orcament.
  COMMIT WORK.

  MODIFY zhcmt_f_orcament FROM TABLE lt_zhcmt_f_orcament.
  COMMIT WORK.

ENDFORM.
