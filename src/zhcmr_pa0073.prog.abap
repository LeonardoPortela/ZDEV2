
*& Report  ZHCMR_PA0073
*& JOB - Processa Comprovantes Pagamento UNIX.
*&---------------------------------------------------------------------*
*& Developer.: Camila Brand
*& Data......: 26.11.2021
*&---------------------------------------------------------------------*

REPORT  zhcmr_pa0073.
TABLES: t001z, lfa1, zfit0091.

TYPES: BEGIN OF ty_filename,
         file TYPE char30.
TYPES: END   OF ty_filename.

DATA: comprovantes_folha TYPE TABLE OF zhcmt_pa_0009,
      gt_file            TYPE TABLE OF string,
      gt_filetab         TYPE TABLE OF sdokpath,
      gt_dirtable        TYPE TABLE OF sdokpath,
      gt_dir_unix        TYPE TABLE OF epsfili,
      gt_dir_unix_e      TYPE TABLE OF epsfili,
      gt_set             TYPE TABLE OF rgsb4,
      wl_set             TYPE rgsb4,
      gt_filename        TYPE TABLE OF ty_filename,
      wl_filename        TYPE ty_filename,

      wl_files_unix      TYPE epsfili,
      comprovante_folha  TYPE zhcmt_pa_0009,
      wl_t001z           TYPE t001z,
      wl_file            TYPE string,

      v_dir_from         LIKE epsf-epsdirnam,
      v_dir_to           LIKE epsf-epsdirnam,
      v_file_from        TYPE string,
      v_file_to          TYPE char100,
      v_erro             TYPE c,
      v_flag(2)          TYPE c VALUE '0',
      v_mot_01(2)        TYPE c,
      v_mot_02(2)        TYPE c,
      v_mot_03(2)        TYPE c,
      v_mot_04(2)        TYPE c,
      v_mot_05(2)        TYPE c.


START-OF-SELECTION.
  SELECT SINGLE COUNT(*) INTO @DATA(vg_job_d)
      FROM tbtco
     WHERE jobname EQ 'PROCESSA_PAGAMENTO_FOLHA_DOCK'
       AND status EQ 'R'.

  IF ( vg_job_d EQ 1 ).
    PERFORM f_processa_arquivos_unix_dock.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_processa_arquivos_unix_dock.

  CLEAR: gt_dir_unix.
  v_dir_from = '/usr/sap/DOCK/RETORNO/'.
  v_dir_to   = '/usr/sap/DOCK/RETORNO/BACKUP/'.

  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = v_dir_from
    TABLES
      dir_list               = gt_dir_unix
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.

  PERFORM f_ler_arquivo_unix.

  CLEAR: gt_dir_unix.
  v_dir_from = '/usr/sap/DOCK/RETORNO/ERRO/'.
  v_dir_to   = '/usr/sap/DOCK/RETORNO/BACKUP/'.

  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = v_dir_from
    TABLES
      dir_list               = gt_dir_unix
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.


  PERFORM f_ler_arquivo_unix.

ENDFORM.                    "LE_ARQUIVO_UNIX

*&---------------------------------------------------------------------*
*& valida nome arquivo
*&---------------------------------------------------------------------*
FORM f_valida_nomearq USING p_name
                   CHANGING p_erro.

  DATA: l_str  TYPE i,
        l_name TYPE char40.

  p_erro = abap_true.

  l_name = p_name.
  TRANSLATE l_name TO UPPER CASE.

  LOOP AT gt_filename INTO wl_filename.
    l_str  = strlen( wl_filename-file ).

    CHECK l_str <> 0.

    IF l_name(l_str) = wl_filename-file.
      p_erro = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_LER_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_ler_arquivo_unix.

  LOOP AT gt_dir_unix INTO wl_files_unix.

*    CLEAR v_erro.
*    PERFORM f_valida_nomearq  USING wl_files_unix-name
*                           CHANGING v_erro.
*
*    CHECK v_erro = abap_false.

    CONCATENATE:
    v_dir_from wl_files_unix-name INTO v_file_from,
    v_dir_to   wl_files_unix-name INTO v_file_to.

    OPEN DATASET v_file_from FOR INPUT IN TEXT MODE ENCODING NON-UNICODE.
    DO.
      TRY.
          READ DATASET v_file_from INTO wl_file.
        CATCH cx_sy_file_open_mode.
          sy-subrc = 4.
      ENDTRY.

      IF ( sy-subrc IS INITIAL ).
        APPEND wl_file TO gt_file.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.
    CLOSE DATASET v_file_from.

    PERFORM f_grava_arquivos.
  ENDLOOP.
ENDFORM.                    "F_LER_ARQUIVO_UNIX


*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_ARQUIVOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_grava_arquivos.
  CLEAR: comprovantes_folha, comprovante_folha, v_flag.

  DATA:
    valor01(1)   TYPE c,
    valor02(1)   TYPE c,
    valor03(1)   TYPE c,
    valor04(1)   TYPE c,
    valor05(1)   TYPE c,
    v_motivo(10) TYPE c.

  "//Grava o nome do arquivo na tabela;
  comprovante_folha-archive = wl_files_unix-name.

  LOOP AT gt_file INTO wl_file.
    IF ( wl_file+8(1) = 'C' ).
      comprovante_folha-banco          = wl_file(3).
      v_flag                           = wl_file+9(2).
      comprovante_folha-agencia        = wl_file+52(6).
      comprovante_folha-conta_corrente = wl_file+58(13).
      comprovante_folha-cnpj_empresa   = wl_file+18(14).

      SELECT SINGLE bukrs
        FROM t001z
        INTO comprovante_folha-bukrs
       WHERE paval = comprovante_folha-cnpj_empresa(8)
        AND bukrs NE '0034'.
    ENDIF.

    CHECK ( v_flag = '30' ).

    CASE wl_file+13(1).
      WHEN 'A'.
        CLEAR:
         comprovante_folha-cpf,
         comprovante_folha-pernr.

        comprovante_folha-bco_fav   = wl_file+20(3).
        comprovante_folha-ag_fav    = wl_file+23(5).
        comprovante_folha-dv_ag_fav = wl_file+28(1).
        comprovante_folha-cc_fav    = wl_file+29(12).
        comprovante_folha-dv_fav    = wl_file+41(1).
        comprovante_folha-vblnr     = wl_file+73(10).

        CONCATENATE wl_file+97(4) wl_file+95(2) wl_file+93(2)
        INTO comprovante_folha-dt_pgto.

        v_mot_01 = wl_file+230(2).
        v_mot_02 = wl_file+232(2).
        v_mot_03 = wl_file+234(2).
        v_mot_04 = wl_file+236(2).
        v_mot_05 = wl_file+238(2).

        SELECT SINGLE pernr
          FROM reguh
          INTO comprovante_folha-pernr
         WHERE vblnr = comprovante_folha-vblnr.

        SELECT SINGLE cpf_nr
          FROM pa0465
          INTO comprovante_folha-cpf
         WHERE pernr = comprovante_folha-pernr.
      WHEN 'Z'.
        comprovante_folha-cod_autent = wl_file+78(25).
        comprovante_folha-tp_proce  = 'A'. "NORMAL
        comprovante_folha-motivo    = v_motivo.
        APPEND comprovante_folha TO comprovantes_folha.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    IF wl_file+13(1) <> 'Z'.
      CASE v_mot_01.
        WHEN  '00' OR '03' OR 'BD' OR 'BE' OR 'BF' OR ' '.
          valor01 = 1.
        WHEN OTHERS.
          valor01 = 2.
          v_motivo = v_mot_01.
      ENDCASE.

      CASE v_mot_02.
        WHEN  '00' OR '03' OR 'BD' OR 'BE' OR 'BF' OR ' '.
          valor02 = 1.
        WHEN OTHERS.
          valor02 = 2.
          IF v_motivo IS NOT INITIAL.
            CONCATENATE: v_mot_01 v_mot_02 INTO v_motivo.
          ELSE.
            v_motivo =  v_mot_02.
          ENDIF.
      ENDCASE.

      CASE v_mot_03.
        WHEN  '00' OR '03' OR 'BD' OR 'BE' OR 'BF' OR ' '.
          valor03 = 1.
        WHEN OTHERS.
          valor03 = 2.
          IF v_motivo IS NOT INITIAL.
            CONCATENATE: v_mot_01 v_mot_02 v_mot_03 INTO v_motivo.
          ELSE.
            v_motivo =  v_mot_03.
          ENDIF.
      ENDCASE.

      CASE v_mot_04.
        WHEN  '00' OR '03' OR 'BD' OR 'BE' OR 'BF' OR ' '.
          valor04 = 1.
        WHEN OTHERS.
          valor04 = 2.
          IF v_motivo IS NOT INITIAL.
            CONCATENATE: v_mot_01 v_mot_02 v_mot_03 v_mot_04 INTO v_motivo.
          ELSE.
            v_motivo =  v_mot_04.
          ENDIF.
      ENDCASE.

      CASE v_mot_05.
        WHEN  '00' OR '03' OR 'BD' OR 'BE' OR 'BF' OR ' '.
          valor05 = 1.
        WHEN OTHERS.
          valor05 = 2.
          IF v_motivo IS NOT INITIAL.
            CONCATENATE: v_mot_01 v_mot_02 v_mot_03 v_mot_04 valor05 INTO v_motivo.
          ELSE.
            v_motivo =  v_mot_05.
          ENDIF.
      ENDCASE.

      IF valor01 = '2' OR  valor02 = '2' OR
         valor03 = '2' OR  valor04 = '2' OR  valor05 = '2'.
        comprovante_folha-tp_proce  = 'R'. "RECUSA
        comprovante_folha-motivo    = v_motivo.
        APPEND comprovante_folha TO comprovantes_folha.
      ENDIF.

    ENDIF.

    CLEAR:  v_mot_01, v_mot_02, v_mot_03, v_mot_04, v_mot_05,
      valor01, valor02, valor03,  valor04,  valor05, v_motivo.

  ENDLOOP.

  DELETE comprovantes_folha WHERE vblnr IS INITIAL.

  MODIFY zhcmt_pa_0009 FROM TABLE comprovantes_folha.
  COMMIT WORK.

  IF v_flag EQ'30'.
    "// Move o arquivo para outro diretório e deleta do atual;
    OPEN DATASET v_file_to FOR OUTPUT IN TEXT MODE ENCODING NON-UNICODE.

    LOOP AT gt_file INTO wl_file.
      TRANSFER wl_file TO v_file_to.
    ENDLOOP.

    CLOSE DATASET v_file_to.

    DELETE DATASET v_file_from.
  ENDIF.

  CLEAR gt_file.

ENDFORM.                    "seleciona_arquivos
