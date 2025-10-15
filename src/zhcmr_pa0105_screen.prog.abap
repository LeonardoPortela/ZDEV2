*&---------------------------------------------------------------------*
*&  Include           ZHCMR_PA0105_SCREEN
*&---------------------------------------------------------------------*


CLASS carga_sf DEFINITION.

  PUBLIC SECTION.

    TYPES: ty_pernr_sel  TYPE RANGE OF pa0001-pernr.

    DATA: at_job_name TYPE btcjob.

    METHODS:

      set_saida,

      set_job_log  IMPORTING etapa_job     TYPE txt50
                             retorno_etapa TYPE txt50
                             sucesso_etapa TYPE char1,

      get_background_job RETURNING VALUE(check_job) TYPE char01.
ENDCLASS.

CLASS carga_sf IMPLEMENTATION.
  METHOD set_saida.

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

    CHECK t_pa001[] IS NOT INITIAL.

    CLEAR: t_saida.
    LOOP AT t_pa001 INTO DATA(wa_pa0001_aux).

      MOVE-CORRESPONDING wa_pa0001_aux TO wa_saida.

      SELECT SINGLE cpf_nr
            FROM pa0465
            INTO @DATA(lv_cpf)
            WHERE endda >= @sy-datum        AND
                  ( cpf_nr <> '' or cpf_nr <> ' ' or cpf_nr is not null ) and
                  pernr = @wa_pa0001_aux-pernr  AND
                  ( pernr <> '' or pernr <> ' ' or pernr is not null ) and
                  subty = '0001'.

      REPLACE ALL OCCURRENCES OF '.' IN lv_cpf WITH space.
      REPLACE ALL OCCURRENCES OF '-' IN lv_cpf WITH space.
      CONDENSE lv_cpf NO-GAPS.

          wa_saida-cpf_nr = lv_cpf.

      SELECT SINGLE cname, begda, gbdat  "++ IR137555 - Add GBDAT
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


      wa_saida-cname   = lv_string_caracteres.

*-- Inicio - CS1092894 - IR137555 - Obter Data de Nascimento do campo GBDAT
*      IF lv_pa0002-begda IS NOT INITIAL.
*        lv_dia = lv_pa0002-begda+6(2).
*        lv_mes = lv_pa0002-begda+4(2).
*        lv_ano = lv_pa0002-begda(4).
*
*        CONCATENATE lv_dia '/'
*                    lv_mes '/'
*                    lv_ano INTO lv_data.
*      ENDIF.

      IF lv_pa0002-gbdat IS NOT INITIAL.
        lv_dia = lv_pa0002-gbdat+6(2).
        lv_mes = lv_pa0002-gbdat+4(2).
        lv_ano = lv_pa0002-gbdat(4).

        CONCATENATE lv_dia '/'
                    lv_mes '/'
                    lv_ano INTO lv_data.
      ENDIF.
*-- Fim - CS1092894 - IR137555

      wa_saida-begda   = lv_data.

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

      wa_saida-dtadmis = lv_data.
      CLEAR lv_pa0002.

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

      wa_saida-gestimed = lv_cpf.

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

      wa_saida-cnameimed = lv_string_caracteres.
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

      wa_saida-areadep = lv_string_caracteres.

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

      CLEAR lv_t001w.
      wa_saida-local = lv_string_caracteres.

      SELECT SINGLE usrid_long
       FROM pa0105
       INTO @DATA(lv_t001w_aux)
       WHERE pernr = @wa_pa0001_aux-pernr AND
             subty = 'MAIL' AND
             endda >= @sy-datum.
      IF sy-subrc IS INITIAL.
        wa_saida-usrid_long = lv_t001w_aux.
      ENDIF.

      APPEND wa_saida TO t_saida.
      CLEAR wa_saida.

    ENDLOOP.

**********************************************************************
* 12-04-2023 Dev - Retirada de CPF duplicados e Matriculas diferentes - PSA
**********************************************************************
DELETE t_saida WHERE cpf_nr is initial.
SORT t_saida by cpf_nr .
DELETE ADJACENT DUPLICATES FROM t_saida COMPARING cpf_nr .




    TYPES:  t_fieldcat          TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

*    CREATE OBJECT obj_container_01 EXPORTING container_name = 'CONTAINER_1'.
*
*    CREATE OBJECT obj_alv_01 EXPORTING i_parent = obj_container_01.

    DATA(tl_fieldcat_dados) = VALUE t_fieldcat(

    ( fieldname = 'CPF_NR'                         coltext = 'CPF'                          col_opt = 'X' )
    ( fieldname = 'PERNR'                          coltext = 'Matrícula'                    col_opt = 'X' )
    ( fieldname = 'CNAME'                          coltext = 'Nome'                         col_opt = 'X' )
    ( fieldname = 'BEGDA'                          coltext = 'Nascimento'                   col_opt = 'X' )
    ( fieldname = 'DTADMIS'                        coltext = 'Admissão'                     col_opt = 'X' )
    ( fieldname = 'STELL'                          coltext = 'Cargo'                        col_opt = 'X' )
    ( fieldname = 'BUKRS'                          coltext = 'Empresa'                      col_opt = 'X' )
    ( fieldname = 'ORGEH'                          coltext = 'Unidade'                      col_opt = 'X' )
    ( fieldname = 'GESTIMED'                       coltext = 'CPF Gestor'                   col_opt = 'X' )
    ( fieldname = 'CNAMEIMED'                      coltext = 'Nome Gestor'                  col_opt = 'X' )
    ( fieldname = 'AREADEP'                        coltext = 'Área Departamento'            col_opt = 'X' )
    ( fieldname = 'LOCAL'                          coltext = 'Localidade'                   col_opt = 'X' )
    ( fieldname = 'USRID_LONG'                     coltext = 'Email'                        col_opt = 'X' )
    ( fieldname = 'STAT2'                          coltext = 'Status'                       col_opt = 'X' ) ).


    DATA(gs_layout) = VALUE lvc_s_layo( sel_mode  = 'A' zebra = 'X' ).

    CALL METHOD obj_alv_01->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        i_save                        = 'A'
      CHANGING
        it_outtab                     = t_saida[]
        it_fieldcatalog               = tl_fieldcat_dados[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDMETHOD.

  METHOD set_job_log.


  ENDMETHOD.

  METHOD get_background_job.

    DATA: v_jobname TYPE btcjob,
          v_step    TYPE btcstepcnt.

    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobname         = v_jobname
        stepcount       = v_step
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.
    IF sy-subrc = 0.
*
      me->at_job_name = v_jobname.
      v_check_job = abap_true.
      check_job = abap_true.

      me->set_job_log( etapa_job = 'Job iniciado' retorno_etapa = |Etapa { v_step }| sucesso_etapa = 'S'  ).

      CASE v_jobname.

        WHEN 'SUCCESS_FTP_DIARIO'.

        WHEN 'SUCCESS_FTP_SABADO'.

      ENDCASE.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-001.

SELECT-OPTIONS so_empre FOR t001-bukrs.

SELECT-OPTIONS so_matri FOR pa0009-pernr.
PARAMETERS: p_data   TYPE datum.

SELECTION-SCREEN END OF BLOCK 1.

SELECTION-SCREEN: SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE text-002.

PARAMETERS: p_ativo TYPE xfeld RADIOBUTTON GROUP 1,
            p_attem TYPE xfeld RADIOBUTTON GROUP 1,
            p_inat  TYPE xfeld RADIOBUTTON GROUP 1.

SELECTION-SCREEN END OF BLOCK 2.
