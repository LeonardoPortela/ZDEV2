*&---------------------------------------------------------------------*
*& Report  ZHCMR_PA0105
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
"REPORT zhcmr_pa0105.

INCLUDE: zhcmr_pa0105_top,
         zhcmr_pa0105_screen.

START-OF-SELECTION.

  IF sy-batch EQ abap_true.
    TRY.
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  IF sy-batch IS INITIAL.
    CREATE OBJECT obj_container_01 EXPORTING container_name = 'CONTAINER_1'.
    CREATE OBJECT obj_alv_01 EXPORTING i_parent = obj_container_01.
  ENDIF.

  DATA(obj_carga) = NEW carga_sf( ).

  PERFORM f_seleciona_dados.

*&---------------------------------------------------------------------*
*&  Include           ZHCMR_PA0105_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  TYPES: BEGIN OF ty_pernr,
           pernr TYPE pa0465-pernr,
         END OF ty_pernr.

  DATA: lt_pernr TYPE TABLE OF ty_pernr.

  DATA: wa_pernr TYPE ty_pernr.

  DATA: r_pernr TYPE RANGE OF pa0001-pernr.
  DATA: wa_pernr_aux LIKE LINE OF r_pernr.

  REFRESH r_empresas_ignoradas[].

  SELECT valsign AS sign
         valoption AS option
         valfrom AS low
    FROM setleaf
    INTO TABLE r_empresas_ignoradas
    WHERE setname = 'ZHCM_EMPRESA_DIALOG'.

  IF p_data IS INITIAL.
    p_data = sy-datum.
  ENDIF.

*** US - 128349 - CBRAND - Inicio
  SELECT *
    FROM tvarvc
  INTO TABLE t_tvarvc
    WHERE name = 'ZHRST_AMAGGIPLAY_AUSEN'.

  LOOP AT t_tvarvc INTO w_tvarvc.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_tvarvc-low ) TO r_subty.
  ENDLOOP.



*** US - 128349 - CBRAND - Fim


  IF p_ativo IS NOT INITIAL.

    SELECT a~pernr
           a~stell
           a~bukrs
           a~werks
           a~orgeh
           b~stat2
           a~endda
      INTO TABLE t_pa001
      FROM pa0001 AS a
      INNER JOIN pa0000 AS b
      ON b~pernr = a~pernr  AND
         b~endda >= p_data AND
         b~stat2 <> '0'
    WHERE a~pernr IN so_matri  AND
          a~bukrs IN so_empre AND
          a~endda >= p_data   AND
          a~abkrs <> 'BA' AND
          a~persk <> 'BB'. "US - 128349 - CBRAND
    IF sy-subrc IS INITIAL.
      SORT t_pa001 BY pernr
                       stell
                       bukrs
                       werks
                       orgeh
                       stat2.
      DELETE ADJACENT DUPLICATES FROM t_pa001 COMPARING ALL FIELDS.
      DELETE t_pa001 WHERE bukrs IN r_empresas_ignoradas.
    ENDIF.


  ELSEIF p_attem IS NOT INITIAL.
    DATA:lva_data     TYPE sy-datum,
         lva_firedate TYPE sy-datum.

    IF p_data IS INITIAL.
      p_data = sy-datum - 2.
    ELSE.
      p_data = p_data - 2.
    ENDIF.
**********************************************************************
* Pega a Matricula da tabela pa0465
**********************************************************************
    SELECT pernr , cpf_nr
      FROM pa0465
      INTO TABLE @DATA(lt_pa0465)
      WHERE begda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0465 BY pernr.
      "DELETE ADJACENT DUPLICATES FROM lt_pa0465 COMPARING CPF_NR.
      DELETE ADJACENT DUPLICATES FROM lt_pa0465 COMPARING ALL FIELDS.
      DELETE ADJACENT DUPLICATES FROM lt_pa0465 COMPARING cpf_nr.

      LOOP AT lt_pa0465 INTO DATA(wa_pa0465).
        wa_pernr-pernr = wa_pa0465-pernr.
        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.
**********************************************************************
* Pega a Matricula da tabela pa0001
**********************************************************************
    SELECT pernr
       FROM pa0001
       INTO TABLE @DATA(lt_pa0001)
       WHERE begda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0001 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0001 COMPARING ALL FIELDS.

      LOOP AT lt_pa0001 INTO DATA(wa_pa0001).
        wa_pernr-pernr = wa_pa0001-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.
**********************************************************************
* Pega a Matricula da tabela pa0002
**********************************************************************
    SELECT pernr
       FROM pa0002
       INTO TABLE @DATA(lt_pa0002)
       WHERE begda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0002 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0002 COMPARING ALL FIELDS.

      LOOP AT lt_pa0002 INTO DATA(wa_pa0002).
        wa_pernr-pernr = wa_pa0002-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.
**********************************************************************
* Pega a Matricula da tabela pa0000
**********************************************************************
    SELECT pernr
       FROM pa0000
       INTO TABLE @DATA(lt_pa0000)
       WHERE begda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0000 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0000 COMPARING ALL FIELDS.

      LOOP AT lt_pa0000 INTO DATA(wa_pa0000).
        wa_pernr-pernr = wa_pa0000-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.
**********************************************************************
* Pega a Matricula da tabela pa0105
**********************************************************************
    SELECT pernr
       FROM pa0105
       INTO TABLE @DATA(lt_pa0105)
       WHERE begda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0105 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0105 COMPARING ALL FIELDS.

      LOOP AT lt_pa0105 INTO DATA(wa_pa0105).
        wa_pernr-pernr = wa_pa0105-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.
**********************************************************************
* Pega a Matricula da tabela pa9002
**********************************************************************
    SELECT pernr
       FROM pa9002
       INTO TABLE @DATA(lt_pa9002)
       WHERE begda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa9002 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa9002 COMPARING ALL FIELDS.

      LOOP AT lt_pa9002 INTO DATA(wa_pa9002).
        wa_pernr-pernr = wa_pa9002-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.

**********************************************************************
* Pega a Matricula Regra Ausencias: US - 128349 - CBRAND
**********************************************************************
    SELECT pernr
       FROM pa2001
       INTO TABLE @DATA(lt_pa2001)
       WHERE begda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa2001 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa2001 COMPARING ALL FIELDS.

      LOOP AT lt_pa2001 INTO DATA(wa_pa2001).
        wa_pernr-pernr = wa_pa2001-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.

**********************************************************************
* Organiza as matriculas
**********************************************************************
    SORT lt_pernr BY pernr.
**********************************************************************
* Deleta matriculas Duplicadas
**********************************************************************
    DELETE ADJACENT DUPLICATES FROM lt_pernr COMPARING pernr.

**********************************************************************
* Deleta matriculas que são safrista - US - 128349
**********************************************************************
    SELECT pernr
       FROM pa0001
       INTO TABLE @DATA(lt_pa001_bb)
       WHERE endda >= @sy-datum
         AND persk = 'BB'.

    IF lt_pa001_bb IS NOT INITIAL.
      LOOP AT lt_pa001_bb INTO DATA(wa_pernr_bb).
        DELETE lt_pernr WHERE pernr = wa_pernr_bb-pernr.
      ENDLOOP.
    ENDIF.

**********************************************************************
* Cria Range Matricula
**********************************************************************

    LOOP AT lt_pernr INTO wa_pernr.

      wa_pernr_aux-sign   = 'I'.
      wa_pernr_aux-option = 'EQ'.
      wa_pernr_aux-low    = wa_pernr-pernr.

      APPEND wa_pernr_aux TO r_pernr.
      CLEAR: wa_pernr_aux.

    ENDLOOP.

**********************************************************************
*
***********************************************************************
    SELECT a~pernr
           a~stell
           a~bukrs
           a~werks
           a~orgeh
           b~stat2
           b~endda
           c~cpf_nr
      INTO TABLE t_pa001
      FROM pa0001 AS a
      INNER JOIN pa0000 AS b
      ON b~pernr = a~pernr  AND
       b~endda >= p_data
      INNER JOIN pa0465 AS c
       ON c~pernr = a~pernr
     WHERE a~pernr  IN so_matri  AND
           a~bukrs  IN so_empre  AND
           a~endda >= p_data     AND
           a~abkrs <> 'BA'       AND
           a~persk <> 'BB'       AND  " US - 128349 - CBRAND
           c~subty = '0001'      AND
           c~cpf_nr <> ''        AND
           c~endda >= sy-datum.

    IF sy-subrc IS INITIAL.

      SORT t_pa001 BY pernr
                      stell
                      bukrs
                      werks
                      orgeh
                    "  stat2
                      endda DESCENDING. "ASCENDING. CSB - 26.01.2023


      DELETE ADJACENT DUPLICATES FROM t_pa001 COMPARING ALL FIELDS.

      DELETE t_pa001 WHERE pernr NOT IN r_pernr.
      DELETE t_pa001 WHERE bukrs IN r_empresas_ignoradas.

      DELETE ADJACENT DUPLICATES FROM t_pa001 COMPARING pernr
                                                        stell
                                                        bukrs
                                                        werks
                                                        orgeh.
*** BUG - 116299 - Inicio - CBRAND
      SORT t_pa001 BY cpf_nr.
      DELETE ADJACENT DUPLICATES FROM t_pa001 COMPARING cpf_nr.
*** BUG - 116299 - Inicio - CBRAND

*** BUG - 100867 - CBRAND - Inicio
      lva_data = sy-datum - 30.
      LOOP AT t_pa001 INTO DATA(lwa_pa001).

        CALL FUNCTION 'RP_GET_FIRE_DATE'
          EXPORTING
            persnr   = lwa_pa001-pernr
          IMPORTING
            firedate = lva_firedate.

        IF lva_firedate IS NOT INITIAL.

          SELECT SINGLE cpf_nr
            FROM pa0465
            INTO @DATA(lv_cpf)
            WHERE endda >= @sy-datum        AND
                  pernr = @lwa_pa001-pernr AND
                  subty = '0001' AND
                  cpf_nr <> ''.

          SELECT pernr,
                 begda " BUG - 167153
            FROM pa0465
          INTO TABLE @DATA(lt_pa0465_aux)
            WHERE pernr <> @lwa_pa001-pernr
             AND cpf_nr = @lv_cpf
             AND subty = '0001' AND
            cpf_nr <> ''.

          IF sy-subrc = 0.

            SORT lt_pa0465_aux BY begda. " BUG - 167153

            LOOP AT lt_pa0465_aux INTO DATA(wa_pa0465_aux).
              CLEAR: lva_firedate.
              CALL FUNCTION 'RP_GET_FIRE_DATE'
                EXPORTING
                  persnr   = wa_pa0465_aux-pernr
                IMPORTING
                  firedate = lva_firedate.

              IF lva_firedate IS INITIAL. "Matricula ainda ativa.
                DELETE t_pa001 WHERE pernr = lwa_pa001-pernr. "Remove o CPF demitido.
                PERFORM f_append_new_pernr USING wa_pa0465_aux-pernr r_empresas_ignoradas[] .
              ELSE.
                IF  lva_firedate > lva_data .
* BUG - 167153 - Inicio - CBRAND
                  "Segunda Matricula encontrada esta demitida - Então envia o desabilitar.
                  "DELETE t_pa001 WHERE pernr = lwa_pa001-pernr. BUG - 14.02.2025 - Matricula 70016038 e 70016039
* BUG - 167153 - Fim - CBRAND
                  PERFORM f_append_new_pernr USING wa_pa0465_aux-pernr r_empresas_ignoradas[] .
                ENDIF.
              ENDIF.
            ENDLOOP.
          ELSE.
            IF  lva_firedate < lva_data .
              DELETE t_pa001 WHERE pernr = lwa_pa001-pernr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

*** BUG - 100867 - CBRAND - Fim
    ENDIF.

  ELSEIF p_inat IS NOT INITIAL.

    SELECT a~pernr
           a~stell
           a~bukrs
           a~werks
           a~orgeh
           b~stat2
           a~endda
    INTO TABLE t_pa001
    FROM pa0001 AS a
    INNER JOIN pa0000 AS b
    ON b~pernr = a~pernr  AND
       b~endda >= p_data AND
       b~stat2 = '0'
    WHERE a~pernr IN so_matri  AND
       a~bukrs IN so_empre AND
       a~endda >= p_data   AND
       a~abkrs <> 'BA' AND
       a~persk <> 'BB'. " US - 128349 - CBRAND

    IF sy-subrc IS INITIAL.

      SORT t_pa001 BY pernr
                      stell
                      bukrs
                      werks
                      orgeh
                      stat2.

      DELETE ADJACENT DUPLICATES FROM t_pa001 COMPARING ALL FIELDS.
      DELETE t_pa001 WHERE bukrs IN r_empresas_ignoradas.
    ENDIF.

  ENDIF.

  IF t_pa001 IS NOT INITIAL.

    DATA(obj_carga) = NEW carga_sf( ).

*    v_check_job = obj_carga->get_background_job( ).

    IF sy-batch IS INITIAL.

      CALL SCREEN 0100.

    ELSE.

*---------------------------------
* integracao
*---------------------------------
      PERFORM f_integra_usuarios.

      COMMIT WORK.

*      ENDLOOP.

      IF t_zhcmt_pa_0035 IS NOT INITIAL.
        MODIFY zhcmt_pa_0035 FROM TABLE t_zhcmt_pa_0035.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
        ENDIF.
      ENDIF.

    ENDIF.

  ELSE.
    MESSAGE 'Sem resultado para os filtros informados!' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
***INCLUDE ZHCMR_PA0105_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: i_tipo_integracao TYPE char2.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
      main_tab-activetab  = c_main_tab-tab1.
      tl_tab  = '0101'.

    WHEN 'MAIN_TAB_TAB1'.
      main_tab-activetab = c_main_tab-tab1.
      tl_tab  = '0101'.

    WHEN 'ENVIA_USER'.

*---------------------------------
* integracao
*---------------------------------
      PERFORM f_integra_usuarios_online.

      COMMIT WORK.

      IF t_zhcmt_pa_0035 IS NOT INITIAL.
        MODIFY zhcmt_pa_0035 FROM TABLE t_zhcmt_pa_0035.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST01'.
  SET TITLEBAR 'T01'.

  IF tl_tab IS INITIAL.
    main_tab-activetab  = c_main_tab-tab1.
    tl_tab = '0101'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  obj_carga->set_saida( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_INTEGRA_USUARIOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_integra_usuarios.

  DATA: l_gbdat     TYPE char10,
        l_hiredate  TYPE char10,
        senha       TYPE string,
        lv_superior TYPE string. "BUG SOLTO 76068

  DATA: lv_begda      TYPE p0000-begda,
        lv_bukrs_text TYPE  butxt.

  DATA: lv_werks_aux  TYPE  t500p-persa,
        lv_werks_text TYPE  t500p-name1.

  DATA: json_input   TYPE string,
        json_retorno TYPE string,
        json         TYPE string.


  DATA: lv_dia  TYPE char2,
        lv_mes  TYPE char2,
        lv_ano  TYPE char4,
        lv_data TYPE string.

  DATA: lv_string_caracteres TYPE string.

  DATA: lv_string TYPE string.

  DATA: lv_count_selecionados TYPE i,
        lv_count              TYPE i.

*----------------------------
* integracao
*----------------------------

  CONCATENATE '{ "employees":[' json_input  INTO json_input.

  DESCRIBE TABLE t_pa001 LINES lv_count_selecionados.

  LOOP AT t_pa001 INTO DATA(wa_pa0001_aux).

    lv_count = lv_count + 1.

    CONCATENATE json_input '{' INTO json_input.

    SELECT SINGLE cpf_nr
      FROM pa0465
      INTO @DATA(lv_cpf)
      WHERE endda >= @sy-datum        AND
            pernr = @wa_pa0001_aux-pernr  AND
            subty = '0001'.

    REPLACE ALL OCCURRENCES OF '.' IN lv_cpf WITH space.
    REPLACE ALL OCCURRENCES OF '-' IN lv_cpf WITH space.
    CONDENSE lv_cpf.

    add_tag 'CPF'    lv_cpf ','.
    CLEAR lv_cpf.
*** BUG - 160360 - Inicio - CBRAND
    add_tag 'Matricula'   wa_pa0001_aux-pernr   ','.
    "add_tag 'Matrícula'   wa_pa0001_aux-pernr   ','.
*** BUG - 160360 - Fim - CBRAND
    SELECT SINGLE cname, begda, gbdat "++ CS1092894 - IR137555 - Add GBDAT
      FROM pa0002
      INTO @DATA(lv_pa0002)
      WHERE endda >= @sy-datum        AND
            pernr = @wa_pa0001_aux-pernr.

    CLEAR lv_string_caracteres.
    lv_string_caracteres = lv_pa0002-cname.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lv_string_caracteres
      IMPORTING
        outtext           = lv_string_caracteres
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
** BUG - 167153 - Inicio
"add_tag 'Nome Completo'           lv_string_caracteres  ','.
    add_tag 'NomeCompleto'           lv_string_caracteres  ','.
** BUG - 167153 - Fim
*-- Inicio - CS1092894 - IR137555 - Obter Data de Nascimento do campo GBDAT
*    IF lv_pa0002-begda IS NOT INITIAL.
*      lv_dia = lv_pa0002-begda+6(2).
*      lv_mes = lv_pa0002-begda+4(2).
*      lv_ano = lv_pa0002-begda(4).
*
*      CONCATENATE lv_dia '/'
*                  lv_mes '/'
*                  lv_ano INTO lv_data.
*    ENDIF.

    IF lv_pa0002-gbdat IS NOT INITIAL.
      lv_dia = lv_pa0002-gbdat+6(2).
      lv_mes = lv_pa0002-gbdat+4(2).
      lv_ano = lv_pa0002-gbdat(4).

      CONCATENATE lv_dia '/'
                  lv_mes '/'
                  lv_ano INTO lv_data.
    ENDIF.
*-- Fim - CS1092894 - IR137555

    IF lv_data IS INITIAL.
      add_tag 'DatadeNascimento'  ''  ','.
    ELSE.
      add_tag 'DatadeNascimento'       lv_data  ','.
    ENDIF.

    CALL FUNCTION 'HR_ENTRY_DATE'
      EXPORTING
        persnr               = wa_pa0001_aux-pernr
        begda                = '18000101'
        endda                = '99991231'
        initialize_ps_buffer = 'X'
      IMPORTING
        entrydate            = lv_begda
      EXCEPTIONS
        entry_date_not_found = 1
        pernr_not_assigned   = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_begda IS NOT INITIAL.
      lv_dia = lv_begda+6(2).
      lv_mes = lv_begda+4(2).
      lv_ano = lv_begda(4).

      CONCATENATE lv_dia '/'
                  lv_mes '/'
                  lv_ano INTO lv_data.
    ENDIF.

    IF lv_data IS INITIAL.
      add_tag 'DatadeAdmissao'   '' ','.
    ELSE.
      add_tag 'DatadeAdmissao'   lv_data ','.
    ENDIF.

    CLEAR lv_pa0002.

    SELECT SINGLE stext
      FROM hrp1000
      INTO @DATA(lv_hrp1000)
      WHERE plvar = '01' AND
            otype = 'C'  AND
            objid = @wa_pa0001_aux-stell AND
            endda >= @sy-datum AND
            langu = 'P'.

    CLEAR lv_string_caracteres.
    lv_string_caracteres = lv_hrp1000.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lv_string_caracteres
      IMPORTING
        outtext           = lv_string_caracteres
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    add_tag 'Cargo'  lv_string_caracteres   ','.
    CLEAR lv_hrp1000.

    CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
      EXPORTING
        bukrs      = wa_pa0001_aux-bukrs
        langu      = 'P'
      IMPORTING
        bukrs_text = lv_bukrs_text.

    CLEAR lv_string_caracteres.
    lv_string_caracteres = lv_bukrs_text.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lv_string_caracteres
      IMPORTING
        outtext           = lv_string_caracteres
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    add_tag 'Empresa'  lv_string_caracteres  ','.

    lv_werks_aux = wa_pa0001_aux-werks.

    CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
      EXPORTING
        werks      = lv_werks_aux
      IMPORTING
        werks_text = lv_werks_text.

    CLEAR lv_string_caracteres.
    lv_string_caracteres = lv_werks_text.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lv_string_caracteres
      IMPORTING
        outtext           = lv_string_caracteres
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    add_tag 'Unidade'  lv_string_caracteres  ','.

    SELECT SINGLE gestimed, cnameimed
      FROM pa9002
      INTO @DATA(lv_pa9002)
      WHERE  pernr  = @wa_pa0001_aux-pernr AND
             endda >= @sy-datum.

    CLEAR lv_cpf.
    lv_cpf = lv_pa9002-gestimed.
    REPLACE ALL OCCURRENCES OF '.' IN lv_cpf WITH space.
    REPLACE ALL OCCURRENCES OF '-' IN lv_cpf WITH space.
    CONDENSE lv_cpf.

    add_tag 'CPFGestor'   lv_cpf  ','.
    CLEAR lv_cpf.
    CLEAR lv_string_caracteres.
    lv_string_caracteres = lv_pa9002-cnameimed.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lv_string_caracteres
      IMPORTING
        outtext           = lv_string_caracteres
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    add_tag 'NomeGestor'  lv_string_caracteres  ','.
    CLEAR lv_pa9002.

    SELECT SINGLE stext
      FROM hrp1000
      INTO @DATA(lv_hrp1000_aux)
      WHERE plvar = '01' AND
            otype = 'O' AND
            objid = @wa_pa0001_aux-orgeh AND
            endda >= @sy-datum AND
            langu = 'P'.

    CLEAR lv_string_caracteres.
    lv_string_caracteres = lv_hrp1000_aux.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lv_string_caracteres
      IMPORTING
        outtext           = lv_string_caracteres
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CLEAR lv_hrp1000_aux.

    add_tag 'AreaDepartamento' lv_string_caracteres ','.

    SELECT SINGLE ort01 , regio
      FROM t001w
      INTO @DATA(lv_t001w)
      WHERE werks = @wa_pa0001_aux-werks.

    CONCATENATE lv_t001w-ort01 lv_t001w-regio INTO lv_string.

    CLEAR lv_string_caracteres.
    lv_string_caracteres = lv_string.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lv_string_caracteres
      IMPORTING
        outtext           = lv_string_caracteres
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    add_tag 'Localidade' lv_string_caracteres ','.

    CLEAR lv_t001w.

    SELECT SINGLE usrid_long
      FROM pa0105
      INTO @DATA(lv_t001w_aux)
      WHERE pernr = @wa_pa0001_aux-pernr AND
            subty = 'MAIL' AND
            endda >= @sy-datum.

    TRANSLATE lv_t001w_aux TO LOWER CASE.
    add_tag 'Email'   lv_t001w_aux   ','.

    CLEAR lv_t001w_aux.


*** US - 128349 - CBRAND - Inicio
    SELECT *
       FROM pa2001
       INTO TABLE @DATA(it_pa2001)
         WHERE pernr = @wa_pa0001_aux-pernr
           AND begda <= @sy-datum
           AND endda >= @sy-datum
           AND subty IN @r_subty.

    IF sy-subrc = 0.
      IF lv_count_selecionados = lv_count.
        add_tag 'Habilitado'           '0'   '}'.
      ELSE.
        add_tag 'Habilitado'           '0'   '},'.
      ENDIF.

    ELSE.
*** US - 128349 - CBRAND - fim
      IF wa_pa0001_aux-stat2 = '0'.

        IF lv_count_selecionados = lv_count.
          add_tag 'Habilitado'           '0'   '}'.
        ELSE.
          add_tag 'Habilitado'           '0'   '},'.
        ENDIF.

      ELSEIF wa_pa0001_aux-stat2 <> '0'.

        IF lv_count_selecionados = lv_count.
          add_tag 'Habilitado'           '1'   '}'.
        ELSE.
          add_tag 'Habilitado'           '1'   '},'.
        ENDIF.

      ENDIF.
    ENDIF.
    SELECT SINGLE *
      FROM zhcmt_pa_0035
      INTO @DATA(wa_pa_0035)
      WHERE pernr  = @wa_pa0001_aux-pernr.
    IF sy-subrc IS INITIAL.
      wa_zhcmt_pa_0035-status          = '2'.
    ELSE.
      wa_zhcmt_pa_0035-status          = '1'.
    ENDIF.

    wa_zhcmt_pa_0035-mandt           = sy-mandt.
    wa_zhcmt_pa_0035-pernr           = wa_pa0001_aux-pernr.
    wa_zhcmt_pa_0035-data_integracao = sy-datum.
    wa_zhcmt_pa_0035-hora_integracao = sy-uzeit.

    IF wa_pa0001_aux-stat2 = '0'.
      wa_zhcmt_pa_0035-status          = '3'.
    ENDIF.

    APPEND wa_zhcmt_pa_0035 TO t_zhcmt_pa_0035.
    CLEAR  wa_zhcmt_pa_0035.

    "Integra dados usuario.

  ENDLOOP.

  CONCATENATE json_input ']}' INTO json_input.

  TRY .

      zcl_integracao_amaggi_play=>zif_integracao_amaggi_play~get_instance(
      )->set_json( e_json =  json_input
      )->set_enviar_usuario( IMPORTING e_json = DATA(json_retorno_aux) ).

      LOOP AT t_zhcmt_pa_0035 ASSIGNING FIELD-SYMBOL(<fs_pa_0035>).
        <fs_pa_0035>-retorno = json_retorno_aux.
        <fs_pa_0035>-code = '200'.
      ENDLOOP.

    CATCH zcx_integracao INTO DATA(ex_integra).

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INTEGRA_USUARIOS_ONLINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_integra_usuarios_online .

  DATA: l_gbdat     TYPE char10,
        l_hiredate  TYPE char10,
        senha       TYPE string,
        lv_superior TYPE string. "BUG SOLTO 76068

  DATA: lv_begda      TYPE p0000-begda,
        lv_bukrs_text TYPE  butxt.

  DATA: lv_string_caracteres TYPE string.

  DATA: lv_werks_aux  TYPE  t500p-persa,
        lv_werks_text TYPE  t500p-name1.

  DATA: json_input   TYPE string,
        json_retorno TYPE string,
        json         TYPE string.


  DATA: lv_dia  TYPE char2,
        lv_mes  TYPE char2,
        lv_ano  TYPE char4,
        lv_data TYPE string.

  DATA: lv_count              TYPE i,
        lv_count_selecionados TYPE i.

  DATA: lv_string TYPE string.

  CALL METHOD obj_alv_01->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows IS INITIAL.

    MESSAGE 'Obrigatório Selecionar a linha' TYPE 'S' DISPLAY LIKE 'E'.

  ELSE.

    CONCATENATE '{ "employees":[' json_input  INTO json_input.

    DESCRIBE TABLE it_sel_rows LINES lv_count_selecionados.

    LOOP AT it_sel_rows INTO DATA(ls_sel_rows).

      lv_count = lv_count + 1.

*      READ TABLE t_pa001 INTO DATA(wa_pa0001) INDEX ls_sel_rows-index.
      READ TABLE t_saida INTO DATA(wa_pa0001) INDEX ls_sel_rows-index.
      IF sy-subrc IS INITIAL.

*----------------------------
* integracao
*----------------------------

        CONCATENATE json_input '{' INTO json_input.

        SELECT SINGLE cpf_nr
          FROM pa0465
          INTO @DATA(lv_cpf)
          WHERE endda >= @sy-datum        AND
                pernr = @wa_pa0001-pernr  AND
                subty = '0001'.

        REPLACE ALL OCCURRENCES OF '.' IN lv_cpf WITH space.
        REPLACE ALL OCCURRENCES OF '-' IN lv_cpf WITH space.
        CONDENSE lv_cpf.

        add_tag 'CPF'    lv_cpf ','.
        CLEAR lv_cpf.
*** BUG - 160360 - Inicio - CBRAND
        add_tag 'Matricula'   wa_pa0001-pernr   ','.
        "add_tag 'Matrícula'   wa_pa0001-pernr   ','.
*** BUG - 160360 - Fim - CBRAND
        SELECT SINGLE cname, begda, gbdat   "++ IR137555 - Add GBDAT
          FROM pa0002
          INTO @DATA(lv_pa0002)
          WHERE endda >= @sy-datum        AND
                pernr = @wa_pa0001-pernr.

        lv_string_caracteres = lv_pa0002-cname.
        CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
          EXPORTING
            intext            = lv_string_caracteres
          IMPORTING
            outtext           = lv_string_caracteres
          EXCEPTIONS
            invalid_codepage  = 1
            codepage_mismatch = 2
            internal_error    = 3
            cannot_convert    = 4
            fields_not_type_c = 5
            OTHERS            = 6.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
** BUG - 167153 - Inicio
        "add_tag 'Nome Completo'           lv_string_caracteres  ','. AJuste dia 19.02.2025
        add_tag 'NomeCompleto'           lv_string_caracteres  ','.
** BUG - 167153 - Fim
*-- Inicio - CS1092894 - IR137555 - Obter Data de Nascimento do campo GBDAT
*        IF lv_pa0002-begda IS NOT INITIAL.
*          lv_dia = lv_pa0002-begda+6(2).
*          lv_mes = lv_pa0002-begda+4(2).
*          lv_ano = lv_pa0002-begda(4).
*
*          CONCATENATE lv_dia '/'
*                      lv_mes '/'
*                      lv_ano INTO lv_data.
*        ENDIF.

        IF lv_pa0002-gbdat IS NOT INITIAL.
        lv_dia = lv_pa0002-gbdat+6(2).
        lv_mes = lv_pa0002-gbdat+4(2).
        lv_ano = lv_pa0002-gbdat(4).

        CONCATENATE lv_dia '/'
                    lv_mes '/'
                    lv_ano INTO lv_data.
      ENDIF.
*-- Fim - CS1092894 - IR137555

      IF lv_data IS INITIAL.
        add_tag 'DatadeNascimento'  ''  ','.
      ELSE.
        add_tag 'DatadeNascimento'       lv_data  ','.
      ENDIF.

      CALL FUNCTION 'HR_ENTRY_DATE'
        EXPORTING
          persnr               = wa_pa0001-pernr
          begda                = '18000101'
          endda                = '99991231'
          initialize_ps_buffer = 'X'
        IMPORTING
          entrydate            = lv_begda
        EXCEPTIONS
          entry_date_not_found = 1
          pernr_not_assigned   = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      IF lv_begda IS NOT INITIAL.
        lv_dia = lv_begda+6(2).
        lv_mes = lv_begda+4(2).
        lv_ano = lv_begda(4).

        CONCATENATE lv_dia '/'
                    lv_mes '/'
                    lv_ano INTO lv_data.
      ENDIF.

      IF lv_data IS INITIAL.
        add_tag 'DatadeAdmissao'   '' ','.
      ELSE.
        add_tag 'DatadeAdmissao'   lv_data ','.
      ENDIF.
      CLEAR lv_pa0002.

      SELECT SINGLE stext
        FROM hrp1000
        INTO @DATA(lv_hrp1000)
        WHERE plvar = '01' AND
              otype = 'C'  AND
              objid = @wa_pa0001-stell AND
              endda >= @sy-datum AND
              langu = 'P'.

      CLEAR lv_string_caracteres.
      lv_string_caracteres = lv_hrp1000.
      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = lv_string_caracteres
        IMPORTING
          outtext           = lv_string_caracteres
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      add_tag 'Cargo'  lv_string_caracteres   ','.
      CLEAR lv_hrp1000.

      CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
        EXPORTING
          bukrs      = wa_pa0001-bukrs
          langu      = 'P'
        IMPORTING
          bukrs_text = lv_bukrs_text.

      CLEAR lv_string_caracteres.
      lv_string_caracteres = lv_bukrs_text.
      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = lv_string_caracteres
        IMPORTING
          outtext           = lv_string_caracteres
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      add_tag 'Empresa'  lv_string_caracteres  ','.


      lv_werks_aux = wa_pa0001-werks.

      CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
        EXPORTING
          werks      = lv_werks_aux
        IMPORTING
          werks_text = lv_werks_text.

      CLEAR lv_string_caracteres.
      lv_string_caracteres = lv_werks_text.
      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = lv_string_caracteres
        IMPORTING
          outtext           = lv_string_caracteres
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      add_tag 'Unidade'  lv_string_caracteres  ','.


      SELECT SINGLE gestimed, cnameimed
        FROM pa9002
        INTO @DATA(lv_pa9002)
        WHERE  pernr  = @wa_pa0001-pernr AND
               endda >= @sy-datum.

      CLEAR lv_cpf.
      lv_cpf = lv_pa9002-gestimed.
      REPLACE ALL OCCURRENCES OF '.' IN lv_cpf WITH space.
      REPLACE ALL OCCURRENCES OF '-' IN lv_cpf WITH space.
      CONDENSE lv_cpf.

      add_tag 'CPFGestor'   lv_cpf  ','.
      CLEAR lv_cpf.

      CLEAR lv_string_caracteres.

      lv_string_caracteres = lv_pa9002-cnameimed.
      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = lv_string_caracteres
        IMPORTING
          outtext           = lv_string_caracteres
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      add_tag 'NomeGestor'  lv_string_caracteres  ','.

      CLEAR lv_pa9002.

      SELECT SINGLE stext
        FROM hrp1000
        INTO @DATA(lv_hrp1000_aux)
        WHERE plvar = '01' AND
              otype = 'O' AND
              objid = @wa_pa0001-orgeh AND
              endda >= @sy-datum AND
              langu = 'P'.

      CLEAR lv_string_caracteres.
      lv_string_caracteres = lv_hrp1000_aux.
      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = lv_string_caracteres
        IMPORTING
          outtext           = lv_string_caracteres
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      add_tag 'AreaDepartamento' lv_string_caracteres ','.

      CLEAR lv_hrp1000_aux.

      SELECT SINGLE ort01 , regio
        FROM t001w
        INTO @DATA(lv_t001w)
        WHERE werks = @wa_pa0001-werks.

      CONCATENATE lv_t001w-ort01 lv_t001w-regio INTO lv_string.

      CLEAR lv_string_caracteres.
      lv_string_caracteres = lv_string.
      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = lv_string_caracteres
        IMPORTING
          outtext           = lv_string_caracteres
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      add_tag 'Localidade' lv_string_caracteres ','.

      CLEAR lv_t001w.

      SELECT SINGLE usrid_long
        FROM pa0105
        INTO @DATA(lv_t001w_aux)
        WHERE pernr = @wa_pa0001-pernr AND
              subty = 'MAIL' AND
              endda >= @sy-datum.
      TRANSLATE lv_t001w_aux TO LOWER CASE.
      add_tag 'Email'   lv_t001w_aux   ','.

      CLEAR lv_t001w_aux.

*** US - 128349 - CBRAND - Inicio
      SELECT *
         FROM pa2001
         INTO TABLE @DATA(it_pa2001)
           WHERE pernr = @wa_pa0001-pernr
             AND begda <= @sy-datum
             AND endda >= @sy-datum
             AND subty IN @r_subty.

      IF sy-subrc = 0.
        IF lv_count_selecionados = lv_count.
          add_tag 'Habilitado'           '0'   '}'.
        ELSE.
          add_tag 'Habilitado'           '0'   '},'.
        ENDIF.

      ELSE.
*** US - 128349 - CBRAND - fim

        IF wa_pa0001-stat2 = '0'.

          IF lv_count_selecionados = lv_count.
            add_tag 'Habilitado'           '0'   '}'.
          ELSE.
            add_tag 'Habilitado'           '0'   '},'.
          ENDIF.

        ELSEIF wa_pa0001-stat2 <> '0'.

          IF lv_count_selecionados = lv_count.
            add_tag 'Habilitado'           '1'   '}'.
          ELSE.
            add_tag 'Habilitado'           '1'   '},'.
          ENDIF.

        ENDIF.
      ENDIF.
      SELECT SINGLE *
        FROM zhcmt_pa_0035
        INTO @DATA(wa_pa_0035)
        WHERE pernr  = @wa_pa0001-pernr.
      IF sy-subrc IS INITIAL.
        wa_zhcmt_pa_0035-status          = '2'.
      ELSE.
        wa_zhcmt_pa_0035-status          = '1'.
      ENDIF.

      wa_zhcmt_pa_0035-mandt           = sy-mandt.
      wa_zhcmt_pa_0035-pernr           = wa_pa0001-pernr.
      wa_zhcmt_pa_0035-data_integracao = sy-datum.
      wa_zhcmt_pa_0035-hora_integracao = sy-uzeit.

      IF wa_pa0001-stat2 = '0'.
        wa_zhcmt_pa_0035-status          = '3'.
      ENDIF.

      APPEND wa_zhcmt_pa_0035 TO t_zhcmt_pa_0035.
      CLEAR  wa_zhcmt_pa_0035.

      "Integra dados usuario.

    ENDIF.
  ENDLOOP.

  CONCATENATE json_input ']}' INTO json_input.

  TRY .

      zcl_integracao_amaggi_play=>zif_integracao_amaggi_play~get_instance(
      )->set_json( e_json =  json_input
      )->set_enviar_usuario( IMPORTING e_json = DATA(json_retorno_aux) ).

      LOOP AT t_zhcmt_pa_0035 ASSIGNING FIELD-SYMBOL(<fs_pa_0035>).
        <fs_pa_0035>-retorno = json_retorno_aux.
        <fs_pa_0035>-code = '200'.
      ENDLOOP.


    CATCH zcx_integracao INTO DATA(ex_integra).

  ENDTRY.

ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_APPEND_NEW_PERNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_append_new_pernr USING p_pernr
                        CHANGING p_r_empresas_ignoradas LIKE r_empresas_ignoradas[].

  CLEAR: t_pa001_aux[].

  READ TABLE t_pa001 INTO DATA(lwa_pa001) WITH KEY pernr = p_pernr.
  IF sy-subrc <> 0.

    SELECT a~pernr
           a~stell
           a~bukrs
           a~werks
           a~orgeh
           b~stat2
           b~endda
    APPENDING TABLE t_pa001_aux
      FROM pa0001 AS a
      INNER JOIN pa0000 AS b
      ON b~pernr = a~pernr  AND
         b~endda >= p_data
     WHERE a~pernr  EQ  p_pernr  AND
           a~endda >= p_data   AND
           a~abkrs <> 'BA'.

      IF sy-subrc = 0.

        SORT t_pa001_aux BY pernr
              stell
              bukrs
              werks
              orgeh
              endda DESCENDING.


        DELETE ADJACENT DUPLICATES FROM t_pa001_aux COMPARING ALL FIELDS.
        DELETE t_pa001_aux WHERE bukrs IN p_r_empresas_ignoradas.
        DELETE ADJACENT DUPLICATES FROM t_pa001_aux COMPARING pernr
                                                              stell
                                                              bukrs
                                                              werks
                                                              orgeh.

        LOOP AT t_pa001_aux INTO DATA(lwa_pa001_aux).
          APPEND lwa_pa001_aux TO t_pa001.
          CLEAR: lwa_pa001_aux.
        ENDLOOP.

      ENDIF.
    ENDIF.
ENDFORM.
