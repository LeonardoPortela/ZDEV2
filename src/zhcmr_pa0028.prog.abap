
*& Report  ZFIS33
*& JOB - Processa Comprovantes Pagamento UNIX.
*&---------------------------------------------------------------------*
*& Developer.: Enio Jesus
*& Data......: 05.10.2016
*&---------------------------------------------------------------------*

REPORT  zhcmr_pa0028.
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
  SELECT SINGLE COUNT(*) INTO @DATA(vg_job)
      FROM tbtco
     WHERE jobname EQ 'PROCESSA_PAGAMENTO_FOLHA'
       AND status EQ 'R'.

  IF ( vg_job EQ 1 ).
    PERFORM f_processa_arquivos_unix.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_processa_arquivos_unix.

*-CS2021000652 - 21.07.2021 - JT - inicio
*-Set com nome dos arquivos
  FREE: gt_set, gt_filename.


  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class           = '0000'
      setnr           = 'ZHCMR_PA0028_NAMEFILES'
      no_descriptions = abap_false
    TABLES
      set_values      = gt_set
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT gt_set     INTO wl_set.
*---> 10/06/2023 - Migração S4 - JS
*       wl_filename-file    = wl_set-from.
  wl_filename-file = CONV #( wl_set-from ).
*<--- 10/06/2023 - Migração S4 - JS

    APPEND wl_filename TO gt_filename.
  ENDLOOP.
*-CS2021000652 - 21.07.2021 - JT - fim

  v_dir_from = '/usr/sap/FINNET/ENTRADA/'.
  v_dir_to   = '/usr/sap/FINNET/ENTRADA/BACKUP_RETORNO/'.

*  V_DIR_FROM = '/usr/sap/trans/PontoQAS/'.
*  V_DIR_TO   = '/usr/sap/trans/PontoQAS/'.


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

*-CS2021000652 - 21.07.2021 - JT - inicio
**    CHECK WL_FILES_UNIX-NAME(3) = 'PFP'.
*    IF wl_files_unix-name(3)   NE 'PFP' AND
*       wl_files_unix-name(3)   NE 'pfp' AND
*       wl_files_unix-name(2)   NE 'FP'  AND
*       wl_files_unix-name(2)   NE 'fp'  AND
*       wl_files_unix-name(9)   NE 'PAG237_10' AND
*       wl_files_unix-name(9)   NE 'PAG001_39'.
*      CONTINUE.
*    ENDIF.

    CLEAR v_erro.
    PERFORM f_valida_nomearq  USING wl_files_unix-name
                           CHANGING v_erro.

    CHECK v_erro = abap_false.
*-CS2021000652 - 21.07.2021 - JT - fim

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

    IF wl_files_unix-name+0(9) NE 'PAG237_10'.

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
*** PBI - 67573 - Inicio - CSB.
          IF wl_files_unix-name+0(5) = 'FP033'.
            comprovante_folha-cod_autent = wl_file+14(64). "ISSUE 72626
            CONDENSE comprovante_folha-cod_autent NO-GAPS.
          ELSE.
            comprovante_folha-cod_autent = wl_file+78(25).
          ENDIF.
*          comprovante_folha-cod_autent = wl_file+78(25).
*** PBI - 67573 - Fim - CSB.
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

    ELSE.

      IF wl_file+0(4) = '0001'.
        comprovante_folha-banco          = '237'.
        comprovante_folha-cnpj_empresa   = wl_file+11(14).

        SELECT SINGLE bukrs
          FROM t001z
          INTO comprovante_folha-bukrs
         WHERE paval = comprovante_folha-cnpj_empresa(8).
      ENDIF.

      CASE wl_file+0(2).
        WHEN '11'.
          CONCATENATE wl_file+2(9) wl_file+15(2) INTO comprovante_folha-cpf.

          comprovante_folha-bco_fav   = wl_file+95(3).
          comprovante_folha-ag_fav    = wl_file+98(5).
          comprovante_folha-dv_ag_fav = wl_file+104(1).
          comprovante_folha-cc_fav    = wl_file+105(13).
          comprovante_folha-dv_fav    = wl_file+117(2).
          comprovante_folha-vblnr     = wl_file+120(10).
          comprovante_folha-dt_pgto   = wl_file+265(8).
          " COMPROVANTE_FOLHA-VLR_PGTO  = WL_FILE+205(13).

          v_mot_01 = wl_file+278(2).
          v_mot_02 = wl_file+280(2).
          v_mot_03 = wl_file+282(2).
          v_mot_04 = wl_file+284(2).
          v_mot_05 = wl_file+286(2).

          comprovante_folha-tp_proce  = 'A'. "RECUSA
          comprovante_folha-motivo    = v_motivo.
          APPEND comprovante_folha TO comprovantes_folha.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      CASE v_mot_01.
        WHEN  'BD' OR 'BU' OR 'BV' OR 'BW' OR 'FS' OR 'HA' OR 'HF' OR 'JL' OR 'LA' OR ' '.
          valor01 = 1.
        WHEN OTHERS.
          valor01 = 2.
          v_motivo = v_mot_01.
      ENDCASE.

      CASE v_mot_02.
        WHEN  'BD' OR 'BU' OR 'BV' OR 'BW' OR 'FS' OR 'HA' OR 'HF' OR 'JL' OR 'LA' OR ' '.
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
        WHEN  'BD' OR 'BU' OR 'BV' OR 'BW' OR 'FS' OR 'HA' OR 'HF' OR 'JL' OR 'LA' OR ' '.
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
        WHEN  'BD' OR 'BU' OR 'BV' OR 'BW' OR 'FS' OR 'HA' OR 'HF' OR 'JL' OR 'LA' OR ' '.
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
        WHEN  'BD' OR 'BU' OR 'BV' OR 'BW' OR 'FS' OR 'HA' OR 'HF' OR 'JL' OR 'LA' OR ' '.
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
      DELETE comprovantes_folha WHERE tp_proce = 'A' AND motivo IS INITIAL.
    ENDIF.
    CLEAR:  v_mot_01, v_mot_02, v_mot_03, v_mot_04, v_mot_05,
            valor01, valor02, valor03,  valor04,  valor05, v_motivo.
  ENDLOOP.

  DELETE comprovantes_folha WHERE vblnr IS INITIAL.

  MODIFY zhcmt_pa_0009 FROM TABLE comprovantes_folha.
  COMMIT WORK.

  IF v_flag EQ'30' OR  ( v_file_from(3)   EQ 'PFP' OR
                         v_file_from(2)   EQ 'FP'  OR
                         v_file_from(9)   EQ 'PAG237_10' ) .

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

*&---------------------------------------------------------------------*
*&      Form  F_AJUSTE_COD_BARRAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_ajuste_cod_barras.

*  DATA LW_ZFIT0091 TYPE ZFIT0091.
*  CLEAR GT_ZFIT0091.
*
*  SELECT *
*    FROM ZHCMT_PA_0009
*    INTO TABLE GT_ZFIT0091
*   WHERE COD_BARRAS NE SPACE.
**     AND DT_PGTO    IN LR_DATA.
*
*  LOOP AT GT_ZFIT0091 INTO LW_ZFIT0091.
*
*    PERFORM F_MONTAR_COD_BARRAS USING
*                                LW_ZFIT0091-BUKRS
*                                LW_ZFIT0091-AUGBL
*                                CHANGING
*                                LW_ZFIT0091-DT_PGTO
*                                LW_ZFIT0091-COD_BARRAS.
*
*    UPDATE ZFIT0091 SET COD_BARRAS = LW_ZFIT0091-COD_BARRAS
*     WHERE BUKRS = LW_ZFIT0091-BUKRS
*       AND AUGBL = LW_ZFIT0091-AUGBL.
*
*    COMMIT WORK.
*
*  ENDLOOP.
ENDFORM.                    "F_AJUSTE_COD_BARRAS

**&---------------------------------------------------------------------*
**&      Form  F_MONTAR_COD_BARRAS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_ZFIT0091 text
**----------------------------------------------------------------------*
*FORM F_MONTAR_COD_BARRAS USING
*                         P_BUKRS     TYPE BUKRS
*                         P_AUGBL     TYPE AUGBL
*                         CHANGING
*                         P_DT_PGTO   TYPE DATUM
*                         P_CODBARRAS TYPE ZCOD_BARRAS.
*
*
*  DATA:
*    LW_BSEG     TYPE BSEG,
*    LV_VALUE    TYPE STRING,
*    LV_COMPLETE TYPE CHAR10,
*    LV_LEN      TYPE I,
*    LV_BSCHL    TYPE CHAR2 VALUE '31'.
*
*  PERFORM F_SELECIONA_BOLETO USING
*                             P_BUKRS
*                             P_AUGBL
*                             CHANGING
*                             P_DT_PGTO
*                             LW_BSEG.
*
*  CHECK ( SY-SUBRC      IS INITIAL ) AND
*    NOT ( LW_BSEG-ESRNR IS INITIAL ) AND
*    NOT ( LW_BSEG-ESRRE IS INITIAL ).
*
*  CLEAR: LV_LEN,
*         LV_COMPLETE.
*
*  LV_VALUE = LW_BSEG-DMBTR.
*
*  REPLACE REGEX '[.]' IN LV_VALUE WITH ''.
*  REPLACE REGEX '[,]' IN LV_VALUE WITH ''.
*
*  "//Completa o código de barras com 0
*  WHILE ( LV_LEN < 48 ).
*    CONCATENATE LW_BSEG-ESRNR LW_BSEG-ESRRE LV_COMPLETE LV_VALUE
*    INTO P_CODBARRAS.
*
*    CONDENSE P_CODBARRAS NO-GAPS.
*
*    LV_LEN = STRLEN( P_CODBARRAS ) + 1.
*    CONCATENATE LV_COMPLETE '0' INTO LV_COMPLETE.
*  ENDWHILE.
*
*  CLEAR: LW_BSEG. "LW_BSAK.
*ENDFORM.                    "F_MONTAR_COD_BARRAS

*FORM F_SELECIONA_PAGAMENTO USING
*                           P_BUKRS   TYPE BUKRS
*                           P_AUGBL   TYPE AUGBL
*                           CHANGING
*                           P_DT_PGTO TYPE DATUM
*                           C_BSEG    TYPE BSEG.
*  SELECT SINGLE *
*    FROM BSAK AS A
*    INTO @DATA(LW_BSAK)
*   WHERE A~BUKRS EQ @P_BUKRS
*     AND A~GJAHR EQ @P_DT_PGTO(4)
*     AND A~AUGBL EQ @P_AUGBL
*     AND A~BELNR NE A~AUGBL.
*
*  IF SY-SUBRC = 0.
*    IF P_DT_PGTO NE LW_BSAK-AUGDT.
*      P_DT_PGTO = LW_BSAK-AUGDT.
*    ENDIF.
*
*    SELECT SINGLE *
*      FROM BSEG
*      INTO C_BSEG
*     WHERE BUKRS EQ LW_BSAK-BUKRS
*       AND BELNR EQ LW_BSAK-BELNR
*       AND GJAHR EQ LW_BSAK-GJAHR
*       AND BSCHL IN ('31','34','37').
*  ENDIF.
*
*  CLEAR LW_BSAK.
*ENDFORM.

*FORM F_SELECIONA_BOLETO USING
*                           P_BUKRS   TYPE BUKRS
*                           P_AUGBL   TYPE AUGBL
*                           CHANGING
*                           P_DT_PGTO TYPE DATUM
*                           C_BSEG    TYPE BSEG.
*  SELECT SINGLE *
*    FROM BSAK AS A
*    INTO @DATA(LW_BSAK)
*   WHERE A~BUKRS EQ @P_BUKRS
*     AND A~GJAHR EQ @P_DT_PGTO(4)
*     AND A~AUGBL EQ @P_AUGBL
*     AND A~BELNR NE A~AUGBL
*     AND A~ZLSCH NOT IN ('U','S').
*
*  IF SY-SUBRC = 0.
*    IF P_DT_PGTO NE LW_BSAK-AUGDT.
*      P_DT_PGTO = LW_BSAK-AUGDT.
*    ENDIF.
*
*    SELECT SINGLE *
*      FROM BSEG
*      INTO C_BSEG
*     WHERE BUKRS EQ LW_BSAK-BUKRS
*       AND BELNR EQ LW_BSAK-BELNR
*       AND GJAHR EQ LW_BSAK-GJAHR
*       AND BSCHL IN ('31','34','37').
*  ENDIF.
*
*  CLEAR LW_BSAK.
*ENDFORM.
*
*FORM F_SELECIONA_FORNECEDOR USING EMPRESA DOCUMENTO DT_PAGAMENTO FORNECEDOR IDENTIFICADOR.
*  IF NOT DOCUMENTO IS INITIAL.
*    SELECT SINGLE LIFNR
*      FROM BSAK
*      INTO FORNECEDOR
*     WHERE BUKRS = EMPRESA
*       AND AUGBL = DOCUMENTO
*       AND AUGDT = DT_PAGAMENTO.
*
*    SELECT SINGLE *
*      FROM LFA1
*      INTO WL_LFA1
*     WHERE LIFNR = FORNECEDOR.
*
*    IF ( WL_LFA1-STKZN = ABAP_TRUE ).
*      IDENTIFICADOR = WL_LFA1-STCD2.
*    ELSE.
*      IDENTIFICADOR = WL_LFA1-STCD1.
*    ENDIF.
*
*  ELSE.
*    "//CNPJ
*    IF STRLEN( IDENTIFICADOR ) > 11.
*      SELECT SINGLE LIFNR
*        FROM LFA1
*        INTO FORNECEDOR
*       WHERE STCD1 = IDENTIFICADOR.
*
*      "//CPF
*    ELSE.
*      SELECT SINGLE LIFNR
*        FROM LFA1
*        INTO FORNECEDOR
*       WHERE STCD2 = IDENTIFICADOR.
*    ENDIF.
*  ENDIF.
*ENDFORM.

*FORM F_SELECIONA_DADOS_BANCARIOS USING EMPRESA AGENCIA CONTA_CORRENTE BANCO.
*  SELECT SINGLE BANKL
*    FROM T012
*    INTO @DATA(_AGENCIA)
*   WHERE BUKRS = @EMPRESA
*     AND HBKID = 'BBD'.
*
*  SELECT SINGLE BANKN, BKONT
*    FROM T012K
*    INTO @DATA(_CC)
*   WHERE BUKRS = @EMPRESA
*     AND HBKID = 'BBD'.
*
*  BANCO   = _AGENCIA(3).
*
*  IF STRLEN( _AGENCIA ) >= 8.
*    AGENCIA = _AGENCIA+4(4) && '1'.
*  ELSE.
*    AGENCIA = _AGENCIA.
*  ENDIF.
*
*  IF ( _CC-BANKN CS '-' ).
*    CONTA_CORRENTE = _CC-BANKN.
*  ELSE.
*    CONTA_CORRENTE = _CC-BANKN && _CC-BKONT.
*  ENDIF.
*ENDFORM.
*
*FORM F_CHECAR_PAGAMENTO USING EMPRESA DOCUMENTO DOC_ESTORNO.
*  CLEAR DOC_ESTORNO.
*
*  SELECT SINGLE STBLG
*    FROM BKPF
*    INTO DOC_ESTORNO
*   WHERE BUKRS = EMPRESA
*     AND BELNR = DOCUMENTO.
*ENDFORM.
