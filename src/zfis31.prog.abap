*&---------------------------------------------------------------------*
*& Report  ZFIS31
*& JOB - Processa Comprovantes Pagamento UNIX.
*&---------------------------------------------------------------------*
*& Developer.: Enio Jesus
*& Data......: 06.11.2015
*&---------------------------------------------------------------------*

REPORT  zfis31.
TABLES: t001z, lfa1, zfit0091.

TYPES: BEGIN OF ty_filename,
         file TYPE char30.
TYPES: END   OF ty_filename.

DATA: gt_zfit0091     TYPE TABLE OF zfit0091,
      gt_file         TYPE TABLE OF string,
      gt_filetab      TYPE TABLE OF sdokpath,
      gt_dirtable     TYPE TABLE OF sdokpath,
      gt_dir_unix     TYPE TABLE OF epsfili,
      gt_set          TYPE TABLE OF rgsb4,
      wl_set          TYPE rgsb4,
      gt_filename     TYPE TABLE OF ty_filename,
      wl_filename     TYPE ty_filename,
*
      wl_files_unix   TYPE epsfili,
      wl_zfit0091     TYPE zfit0091,
      wl_zfit0091_aux TYPE zfit0091,
      wl_t001z        TYPE t001z,
      wl_lfa1         TYPE lfa1,
      wl_file         TYPE string,
      wl_bsak         TYPE bsak,

      v_dir_from      LIKE epsf-epsdirnam,
      v_dir_to        LIKE epsf-epsdirnam,
      v_file_from     TYPE string,
      v_file_to       TYPE char100,
      v_erro          TYPE c,
      vl_augbl        TYPE augbl,
      vl_cod_barras   TYPE augbl,
      v_flag(2)       TYPE c VALUE '0'.

START-OF-SELECTION.

*  MESSAGE s024(sd) WITH 'Check JOB ativo'.

  SELECT SINGLE COUNT(*) INTO @DATA(vg_job)
      FROM tbtco
     WHERE jobname EQ 'PROCESSA_COMPROV_FINNET'
       AND status EQ 'R'.

  IF ( vg_job EQ 1 ).

*    MESSAGE s024(sd) WITH 'Inicio FORM processa arquivo unix'.
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
      setnr           = 'ZFIS31_NAMEFILES'
      no_descriptions = abap_false
    TABLES
      set_values      = gt_set
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT gt_set     INTO wl_set.
    wl_filename-file    = wl_set-from.
    APPEND wl_filename TO gt_filename.
  ENDLOOP.
*-CS2021000652 - 21.07.2021 - JT - fim

  v_dir_from = '/usr/sap/FINNET/ENTRADA/'.
  v_dir_to   = '/usr/sap/FINNET/ENTRADA/BACKUP_RETORNO/'.

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

  FREE: p_erro.

  l_name = p_name.
  TRANSLATE l_name TO UPPER CASE.

  LOOP AT gt_filename INTO wl_filename.
    l_str  = strlen( wl_filename-file ).

    CHECK l_str <> 0.

    IF l_name(l_str) = wl_filename-file.
      p_erro = abap_true.
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
  DATA: vg_lines TYPE p.

  CLEAR: vg_lines.
  DESCRIBE TABLE gt_dir_unix LINES vg_lines.

  MESSAGE s024(sd) WITH 'Inico do processo para ler dados de cada arquivo' 'Total: ' vg_lines.

  LOOP AT gt_dir_unix INTO wl_files_unix.

    DATA(vg_tabix) = sy-tabix.
    MESSAGE s024(sd) WITH 'Processando dados ' vg_tabix 'de um total' vg_lines.

    MESSAGE s024(sd) WITH wl_files_unix-name.

*-CS2021000652 - 21.07.2021 - JT - inicio
*   CHECK wl_files_unix-name(3) <> 'PFP' AND wl_files_unix-name(2) <> 'FP' AND wl_files_unix-name(9) <> 'PAG237_10'.
*   CHECK wl_files_unix-name(4) <> 'COBR'.

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
*      MESSAGE s024(sd) WITH wl_file.

      TRY.
          READ DATASET v_file_from INTO wl_file.
        CATCH cx_sy_file_open_mode.
          sy-subrc = 4.
      ENDTRY.

      IF ( sy-subrc IS INITIAL ).
        APPEND wl_file TO gt_file.
      ELSE.
*        MESSAGE s024(sd) WITH 'Sair do loop do/enddo'.
        EXIT.
      ENDIF.

    ENDDO.
    CLOSE DATASET v_file_from.

    PERFORM f_grava_arquivos.

*    MESSAGE s024(sd) WITH 'Fim do processamento dos dados' vg_tabix ' / ' wl_files_unix-name.
  ENDLOOP.
  MESSAGE s024(sd) WITH 'Fim do processamento total'.

ENDFORM.                    "F_LER_ARQUIVO_UNIX


*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_ARQUIVOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_grava_arquivos.
  CLEAR: gt_zfit0091, wl_zfit0091, v_flag.

  "//Grava o nome do arquivo na tabela;
*  MESSAGE s024(sd) WITH 'Inicia FORM grava arquivo.'.
  wl_zfit0091-archive = wl_files_unix-name.


  LOOP AT gt_file INTO wl_file.

    IF ( wl_zfit0091-bukrs IS INITIAL ).
      SELECT SINGLE bukrs
        FROM t001z
        INTO wl_zfit0091-bukrs
       WHERE paval = wl_file+11(8)
        AND bukrs NE '0034'.
    ENDIF.

    "//Processamento banco bradesco
    IF ( ( wl_zfit0091-bukrs = '0038' ) OR ( wl_zfit0091-bukrs = '0035' ) AND wl_zfit0091-banco = '237' ).

      IF wl_zfit0091-cnpj_empresa IS INITIAL.
        wl_zfit0091-cnpj_empresa = wl_file+11(14).

        PERFORM f_seleciona_dados_bancarios
         USING
           wl_zfit0091-bukrs
           wl_zfit0091-agencia
           wl_zfit0091-conta_corrente
           wl_zfit0091-banco.

      ELSE.
        wl_zfit0091-bco_fav      = wl_file+95(3).
        wl_zfit0091-ag_fav       = wl_file+98(5).
        wl_zfit0091-dv_ag_fav    = wl_file+103(1).
        wl_zfit0091-cc_fav       = wl_file+104(13).
        wl_zfit0091-dv_fav       = wl_file+117(2).
        wl_zfit0091-augbl        = wl_file+119(16).
        wl_zfit0091-cod_autent   = wl_file+119(16).
        wl_zfit0091-vlr_pgto     = ( wl_file+204(15) / 100 ).
        wl_zfit0091-dt_pgto      = wl_file+165(8).

        IF wl_file+1(1) = '2'. "//Juridica
          wl_zfit0091-cnpj_cpf_forn = wl_file+3(14).
        ELSE.                      "//Física
          wl_zfit0091-cnpj_cpf_forn = wl_file+2(9) && wl_file+15(2).
        ENDIF.

        IF ( wl_zfit0091-augbl IS INITIAL ).
          CLEAR wl_zfit0091.
          EXIT.
        ENDIF.

        PERFORM f_seleciona_fornecedor
         USING
           space
           space
           space
           wl_zfit0091-lifnr
           wl_zfit0091-cnpj_cpf_forn.

        DATA doc_estorno TYPE bkpf-stblg.

        PERFORM f_checar_pagamento
         USING
           wl_zfit0091-bukrs
           wl_zfit0091-augbl
           doc_estorno.

        IF ( doc_estorno IS INITIAL ).
          CLEAR wl_zfit0091-cod_barras.

          PERFORM f_montar_cod_barras
           USING
             wl_zfit0091-bukrs
             wl_zfit0091-augbl
           CHANGING
             wl_zfit0091-dt_pgto
             wl_zfit0091-cod_barras.

          IF wl_zfit0091-cod_barras IS NOT INITIAL.
            CLEAR: wl_zfit0091-ag_fav,
                   wl_zfit0091-dv_ag_fav,
                   wl_zfit0091-cc_fav,
                   wl_zfit0091-dv_fav.
          ENDIF.

          APPEND wl_zfit0091 TO gt_zfit0091.
        ELSE.
          CLEAR doc_estorno.
        ENDIF.
      ENDIF.

    ELSE.
      "//Processamento demais bancos
      IF ( wl_file+8(1) = 'C' ).
        wl_zfit0091-banco          = wl_file(3).
        v_flag                     = wl_file+9(2).
        wl_zfit0091-agencia        = wl_file+52(6).
        wl_zfit0091-conta_corrente = wl_file+58(13).
        wl_zfit0091-cnpj_empresa   = wl_file+18(14).

        SELECT SINGLE bukrs
          FROM t001z
          INTO wl_zfit0091-bukrs
         WHERE paval = wl_zfit0091-cnpj_empresa(8)
          AND bukrs NE '0034'.
      ENDIF.

      CHECK ( v_flag = '20' ) OR ( v_flag = '98' ) OR ( v_flag = '22' ) . "22 - CNAB BARDESCO - BUG - 90395 - CBRAND

      CASE wl_file+13(1).
        WHEN 'A'.
          CLEAR: wl_zfit0091-cod_barras,
                 wl_zfit0091-cnpj_cpf_forn,
                 wl_zfit0091-lifnr.

          wl_zfit0091-bco_fav      = wl_file+20(3).
          wl_zfit0091-ag_fav       = wl_file+23(5).
          wl_zfit0091-dv_ag_fav    = wl_file+28(1).
          wl_zfit0091-cc_fav       = wl_file+29(12).
          wl_zfit0091-dv_fav       = wl_file+41(1).
          wl_zfit0091-augbl        = wl_file+73(10).
          wl_zfit0091-vlr_pgto     = ( wl_file+119(15) / 100 ).

          CONCATENATE wl_file+97(4) wl_file+95(2) wl_file+93(2)
          INTO wl_zfit0091-dt_pgto.

          PERFORM f_seleciona_fornecedor
           USING
             wl_zfit0091-bukrs
             wl_zfit0091-augbl
             wl_zfit0091-dt_pgto
             wl_zfit0091-lifnr
             wl_zfit0091-cnpj_cpf_forn.

        WHEN 'J'.

          IF wl_file+17(2) EQ '52'. "Ajuste referente IR173539 / 13-03-2024 / AOENNING.
            CONTINUE.
          ENDIF.

          vl_augbl = wl_file+182(10).

          IF  vl_augbl IS INITIAL.
            CONTINUE.
          ENDIF.

          CLEAR: wl_zfit0091-cod_barras,
                 wl_zfit0091-cnpj_cpf_forn,
                 wl_zfit0091-lifnr.

          CONCATENATE wl_file+148(4) wl_file+146(2) wl_file+144(2)
          INTO wl_zfit0091-dt_pgto.

          wl_zfit0091-vlr_pgto     = ( wl_file+152(15) / 100 ).
          wl_zfit0091-augbl        = wl_file+182(10).


          PERFORM f_montar_cod_barras
           USING
             wl_zfit0091-bukrs
             wl_zfit0091-augbl
           CHANGING
             wl_zfit0091-dt_pgto
             wl_zfit0091-cod_barras.

          PERFORM f_seleciona_fornecedor
           USING
             wl_zfit0091-bukrs
             wl_zfit0091-augbl
             wl_zfit0091-dt_pgto
             wl_zfit0091-lifnr
             wl_zfit0091-cnpj_cpf_forn.

        WHEN 'O'. "Tributos CNAB - BUG SOLTO 83647

          vl_augbl = wl_file+122(10).
          vl_cod_barras = vl_augbl.

          IF  vl_augbl IS INITIAL.
            CONTINUE.
          ENDIF.

          CLEAR: wl_zfit0091-cod_barras,
                 wl_zfit0091-cnpj_cpf_forn,
                 wl_zfit0091-lifnr.

          PERFORM f_doc_contabil USING vl_augbl wl_zfit0091-bukrs CHANGING vl_augbl.

          CONCATENATE wl_file+103(4) wl_file+101(2) wl_file+99(2)
          INTO wl_zfit0091-dt_pgto.

          wl_zfit0091-vlr_pgto     = ( wl_file+107(15) / 100 ).
          wl_zfit0091-augbl        = vl_augbl.

          IF  v_flag = '22' AND  wl_zfit0091-banco = '237'. "Bradesco *US 90395 - CBRAND

            SELECT SINGLE cod_barras
              FROM zimp_lanc_impost INTO wl_zfit0091-cod_barras
            WHERE doc_imposto = vl_cod_barras
              AND bukrs       = wl_zfit0091-bukrs.

          ELSE.

            PERFORM f_montar_cod_barras
             USING
               wl_zfit0091-bukrs
               wl_zfit0091-augbl
             CHANGING
               wl_zfit0091-dt_pgto
               wl_zfit0091-cod_barras.

          ENDIF.

          PERFORM f_seleciona_fornecedor
           USING
             wl_zfit0091-bukrs
             wl_zfit0091-augbl
             wl_zfit0091-dt_pgto
             wl_zfit0091-lifnr
             wl_zfit0091-cnpj_cpf_forn.

          " Início - DEVK9A1POD - RSA - 28/09/2023
          IF wl_zfit0091-lifnr IS INITIAL.
            PERFORM f_get_fornecedor_cnpj USING wl_zfit0091-bukrs wl_zfit0091-augbl wl_zfit0091-lifnr wl_zfit0091-cnpj_cpf_forn.
          ENDIF.

          IF wl_zfit0091-cod_barras IS INITIAL.
            wl_zfit0091-cod_barras = wl_file+17(44).
          ENDIF.
          " Fim - DEVK9A1POD - RSA - 28/09/2023

        WHEN 'Z'.

          wl_zfit0091-cod_autent = wl_file+78(25).
          APPEND wl_zfit0091 TO gt_zfit0091.
          CLEAR wl_zfit0091-augbl.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDIF.
  ENDLOOP.



  IF gt_zfit0091[] IS NOT INITIAL.  "BUG - 90395 - CBRAND
    DELETE gt_zfit0091 WHERE augbl IS INITIAL.

*    MESSAGE s024(sd) WITH 'Salvar dados na tabela ZFIT0091'.
    MODIFY zfit0091 FROM TABLE gt_zfit0091.
    COMMIT WORK.
  ENDIF.
  "// Move o arquivo para outro diretório e deleta do atual;
  IF v_flag NE'30'.
    OPEN DATASET v_file_to FOR OUTPUT IN TEXT MODE ENCODING NON-UNICODE.

    LOOP AT gt_file INTO wl_file.
      TRANSFER wl_file TO v_file_to.
    ENDLOOP.

    CLOSE DATASET v_file_to.

*    DELETE DATASET V_FILE_FROM.
  ENDIF.
  CLEAR gt_file.

ENDFORM.                    "seleciona_arquivos

*&---------------------------------------------------------------------*
*&      Form  F_AJUSTE_COD_BARRAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_ajuste_cod_barras.

  DATA lw_zfit0091 TYPE zfit0091.
  CLEAR gt_zfit0091.

  SELECT *
    FROM zfit0091
    INTO TABLE gt_zfit0091
   WHERE cod_barras NE space.
*     AND DT_PGTO    IN LR_DATA.

  LOOP AT gt_zfit0091 INTO lw_zfit0091.

    PERFORM f_montar_cod_barras USING
                                lw_zfit0091-bukrs
                                lw_zfit0091-augbl
                                CHANGING
                                lw_zfit0091-dt_pgto
                                lw_zfit0091-cod_barras.

    UPDATE zfit0091 SET cod_barras = lw_zfit0091-cod_barras
     WHERE bukrs = lw_zfit0091-bukrs
       AND augbl = lw_zfit0091-augbl.

    COMMIT WORK.

  ENDLOOP.
ENDFORM.                    "F_AJUSTE_COD_BARRAS

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_COD_BARRAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZFIT0091 text
*----------------------------------------------------------------------*
FORM f_montar_cod_barras USING
                         p_bukrs     TYPE bukrs
                         p_augbl     TYPE augbl
                         CHANGING
                         p_dt_pgto   TYPE datum
                         p_codbarras TYPE zcod_barras.


  DATA:
    lw_bseg     TYPE bseg,
    lv_value    TYPE string,
    lv_complete TYPE char10,
    lv_len      TYPE i,
    lv_bschl    TYPE char2 VALUE '31'.

*  MESSAGE s024(sd) WITH 'Monta condigo barra'.

  PERFORM f_seleciona_boleto USING
                             p_bukrs
                             p_augbl
                             CHANGING
                             p_dt_pgto
                             lw_bseg.


  CHECK ( sy-subrc      IS INITIAL ) AND
    NOT ( lw_bseg-esrnr IS INITIAL ) AND
    NOT ( lw_bseg-esrre IS INITIAL ).

  CLEAR: lv_len,
         lv_complete.

  lv_value = lw_bseg-dmbtr.

  REPLACE REGEX '[.]' IN lv_value WITH ''.
  REPLACE REGEX '[,]' IN lv_value WITH ''.

  "//Completa o código de barras com 0

*  MESSAGE s024(sd) WITH 'Check código de barra' 'WHILE/ENDWHILE'.
  WHILE ( lv_len < 48 ).
    CONCATENATE lw_bseg-esrnr lw_bseg-esrre lv_complete lv_value
    INTO p_codbarras.
    CONDENSE p_codbarras NO-GAPS.

*    MESSAGE s024(sd) WITH 'Verificando código de barra' p_codbarras.

    lv_len = strlen( p_codbarras ) + 1.
    CONCATENATE lv_complete '0' INTO lv_complete.
  ENDWHILE.

*  MESSAGE s024(sd) WITH 'Fim do check código de barra WHILE/ENDWHILE'.


  CLEAR: lw_bseg. "LW_BSAK.
ENDFORM.                    "F_MONTAR_COD_BARRAS

FORM f_seleciona_pagamento USING
                           p_bukrs   TYPE bukrs
                           p_augbl   TYPE augbl
                           CHANGING
                           p_dt_pgto TYPE datum
                           c_bseg    TYPE bseg.
  SELECT SINGLE *
    FROM bsak AS a
    INTO @DATA(lw_bsak)
   WHERE a~bukrs EQ @p_bukrs
     AND a~gjahr EQ @p_dt_pgto(4)
     AND a~augbl EQ @p_augbl
     AND a~belnr NE a~augbl.

  IF sy-subrc = 0.
    IF p_dt_pgto NE lw_bsak-augdt.
      p_dt_pgto = lw_bsak-augdt.
    ENDIF.

    SELECT SINGLE *
      FROM bseg
      INTO c_bseg
     WHERE bukrs EQ lw_bsak-bukrs
       AND belnr EQ lw_bsak-belnr
       AND gjahr EQ lw_bsak-gjahr
       AND bschl IN ('31','34','37').
  ENDIF.

  CLEAR lw_bsak.
ENDFORM.

FORM f_seleciona_boleto USING
                           p_bukrs   TYPE bukrs
                           p_augbl   TYPE augbl
                           CHANGING
                           p_dt_pgto TYPE datum
                           c_bseg    TYPE bseg.

*  MESSAGE s024(sd) WITH 'Seleção de dados tabela bsak'.

  SELECT SINGLE *
    FROM bsak AS a
    INTO @DATA(lw_bsak)
   WHERE a~bukrs EQ @p_bukrs
     AND a~gjahr EQ @p_dt_pgto(4)
     AND a~augbl EQ @p_augbl
     AND a~belnr NE a~augbl
     AND a~zlsch NOT IN ('U','S','X').

  IF sy-subrc = 0.
    IF p_dt_pgto NE lw_bsak-augdt.
      p_dt_pgto = lw_bsak-augdt.
    ENDIF.

*    MESSAGE s024(sd) WITH 'Seleção de dados tabela bseg'.

    SELECT SINGLE *
      FROM bseg
      INTO c_bseg
     WHERE bukrs EQ lw_bsak-bukrs
       AND belnr EQ lw_bsak-belnr
       AND gjahr EQ lw_bsak-gjahr
       AND bschl IN ('31','34','37').
  ENDIF.

  CLEAR lw_bsak.
ENDFORM.

FORM f_seleciona_fornecedor USING empresa documento dt_pagamento fornecedor identificador.
  IF NOT documento IS INITIAL.
    SELECT SINGLE lifnr
      FROM bsak
      INTO fornecedor
     WHERE bukrs = empresa
       AND augbl = documento
       AND augdt = dt_pagamento.

    SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
     WHERE lifnr = fornecedor.

    IF ( wl_lfa1-stkzn = abap_true ).
      identificador = wl_lfa1-stcd2.
    ELSE.
      identificador = wl_lfa1-stcd1.
    ENDIF.

  ELSE.
    "//CNPJ
    IF strlen( identificador ) > 11.
      SELECT SINGLE lifnr
        FROM lfa1
        INTO fornecedor
       WHERE stcd1 = identificador.

      "//CPF
    ELSE.
      SELECT SINGLE lifnr
        FROM lfa1
        INTO fornecedor
       WHERE stcd2 = identificador.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_seleciona_dados_bancarios USING empresa agencia conta_corrente banco.
  SELECT SINGLE bankl
    FROM t012
    INTO @DATA(_agencia)
   WHERE bukrs = @empresa
     AND hbkid = 'BBD'.

  SELECT SINGLE bankn, bkont
    FROM t012k
    INTO @DATA(_cc)
   WHERE bukrs = @empresa
     AND hbkid = 'BBD'.

  banco   = _agencia(3).

  IF strlen( _agencia ) >= 8.
    agencia = _agencia+4(4) && '1'.
  ELSE.
    agencia = _agencia.
  ENDIF.

  IF ( _cc-bankn CS '-' ).
    conta_corrente = _cc-bankn.
  ELSE.
    conta_corrente = _cc-bankn && _cc-bkont.
  ENDIF.
ENDFORM.

FORM f_checar_pagamento USING empresa documento doc_estorno.
  CLEAR doc_estorno.

  SELECT SINGLE stblg
    FROM bkpf
    INTO doc_estorno
   WHERE bukrs = empresa
     AND belnr = documento.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DOC_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_AUGBL  text
*      <--P_VL_AUGBL  text
*----------------------------------------------------------------------*
FORM f_doc_contabil  USING    p_vl_augbl
                              p_bukrs
                     CHANGING p_augbl.
  TYPES:
    BEGIN OF ty_zib_contabil_chv,
      obj_key TYPE zib_contabil_chv-obj_key,
      belnr   TYPE zib_contabil_chv-belnr,
      bukrs   TYPE zib_contabil_chv-bukrs,
      gjahr   TYPE zib_contabil_chv-gjahr,
    END OF ty_zib_contabil_chv.

  DATA: vobj_key            TYPE zib_contabil-obj_key,
        wa_zib_contabil_chv TYPE ty_zib_contabil_chv.

  CLEAR: vobj_key.
  CONCATENATE 'ZP' p_bukrs p_vl_augbl '%' INTO vobj_key.
  SELECT SINGLE obj_key belnr bukrs gjahr
    FROM zib_contabil_chv INTO wa_zib_contabil_chv
    WHERE obj_key LIKE vobj_key
    AND   bukrs EQ p_bukrs.

  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM bsak INTO @DATA(ws_bsak) WHERE bukrs EQ @p_bukrs AND belnr EQ @wa_zib_contabil_chv-belnr.
    IF sy-subrc EQ 0.
      p_augbl = ws_bsak-augbl.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_FORNECEDOR_CNPJ
*&---------------------------------------------------------------------*
FORM f_get_fornecedor_cnpj  USING empresa documento fornecedor identificador.

  RANGES: rg_bschl FOR bseg-bschl.

  rg_bschl-sign   = 'I'.
  rg_bschl-option = 'EQ'.
  rg_bschl-low    = '31'.
  APPEND rg_bschl.

  rg_bschl-sign   = 'I'.
  rg_bschl-option = 'EQ'.
  rg_bschl-low    = '39'.
  APPEND rg_bschl.


  IF NOT documento IS INITIAL.

    SELECT lifnr UP TO 1 ROWS
      FROM bseg
      INTO fornecedor
     WHERE bukrs = empresa
       AND belnr = documento
       AND bschl IN rg_bschl.
    ENDSELECT.

    IF NOT fornecedor IS INITIAL.

      SELECT SINGLE *
        FROM lfa1
        INTO wl_lfa1
       WHERE lifnr = fornecedor.

      IF ( wl_lfa1-stkzn = abap_true ).
        identificador = wl_lfa1-stcd2.
      ELSE.
        identificador = wl_lfa1-stcd1.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
