*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZMMI0001                                                *
* Descrição  : Importação de Dados Classificação HVI                   *
* Módulo     : MM                                Transação: ZMM0024    *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 26/07/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      : Igor Sobral                            Data: 21/06/2013 *
* Observações: Adicionar Safra e Periodo                               *
*----------------------------------------------------------------------*
REPORT zmmi0001 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: t001w, mara, ztsafrafardos.

CONSTANTS: heade_kuhlmann  TYPE string VALUE 'Numer',
           heade_unicotton TYPE string VALUE 'FARDO',
           heade_hvi       TYPE string VALUE 'Peso',
           c_kuhlmann      TYPE c VALUE 'K',
           c_unicotton     TYPE c VALUE 'U',
           c_hvi           TYPE c VALUE 'H'. "CS2022000332-#78805-19.07.2022-JT

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_data,
         campo TYPE char1024,
       END   OF type_data,

       BEGIN OF type_mchb,
         matnr TYPE mchb-matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         charg TYPE mchb-charg,
         objek TYPE inob-objek,
       END   OF type_mchb,

       BEGIN OF type_rmclm,
         klart TYPE tcla-klart,
         cuobj TYPE inob-cuobj,
         clint TYPE kssk-clint,
         class TYPE klah-class,
         vondt TYPE klah-vondt,
         bisdt TYPE klah-bisdt,
       END   OF type_rmclm,

       BEGIN OF type_inob,
         cuobj  TYPE inob-cuobj,
         klart  TYPE inob-klart,
         obtab  TYPE inob-obtab,
         objek  TYPE inob-objek,
         objekk TYPE kssk-objek,
       END   OF type_inob,

       BEGIN OF type_kssk,
         objek TYPE kssk-objek,
         mafid TYPE kssk-mafid,
         klart TYPE kssk-klart,
         clint TYPE kssk-clint,
         adzhl TYPE kssk-adzhl,
       END   OF type_kssk,

       BEGIN OF type_klah,
         clint TYPE klah-clint,
         klart TYPE klah-klart,
         class TYPE klah-class,
         vondt TYPE klah-vondt,
         bisdt TYPE klah-bisdt,
       END   OF type_klah,

       BEGIN OF type_msg,
         material TYPE mchb-matnr,
         lote     TYPE mchb-charg,
         centro   TYPE mchb-werks,
         tipo     TYPE char1,
         mesagem  TYPE char40,
       END   OF type_msg,

       BEGIN OF type_ksml,
         clint TYPE ksml-clint,
         posnr TYPE ksml-posnr,
         adzhl TYPE ksml-adzhl,
         imerk TYPE ksml-imerk,
         klart TYPE ksml-klart,
       END   OF type_ksml,

       BEGIN OF type_cabnt,
         atinn TYPE cabnt-atinn,
         spras TYPE cabnt-spras,
         adzhl TYPE cabnt-adzhl,
         atbez TYPE cabnt-atbez,
         atnam TYPE cabn-atnam,
         atfor TYPE cabn-atfor,
       END   OF type_cabnt,

       BEGIN OF ty_calc,

         c_str   TYPE p DECIMALS 3,
         c_uhm   TYPE p DECIMALS 3,
         c_ui	   TYPE p DECIMALS 3,
         c_rd    TYPE p DECIMALS 3,
         c_b     TYPE p DECIMALS 3,
         c_trash TYPE p DECIMALS 3,
         c_mic   TYPE p DECIMALS 3,

         c_fixo  TYPE p DECIMALS 3,

         v_str   TYPE p DECIMALS 3,
         v_uhm   TYPE p DECIMALS 3,
         v_uhm_1 TYPE p DECIMALS 3,
         v_uhm_2 TYPE p DECIMALS 3,
         v_ui	   TYPE p DECIMALS 3,
         v_rd    TYPE p DECIMALS 3,
         v_b     TYPE p DECIMALS 3,
         v_trash TYPE p DECIMALS 3,
         v_mic   TYPE p DECIMALS 3,

         r_str   TYPE p DECIMALS 3,
         r_uhm   TYPE p DECIMALS 3,
         r_ui    TYPE p DECIMALS 3,
         r_rd    TYPE p DECIMALS 3,
         r_b     TYPE p DECIMALS 3,
         r_trash TYPE p DECIMALS 3,
         r_mic   TYPE p DECIMALS 3,

         f_csp   TYPE p DECIMALS 2,

       END OF ty_calc.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_data         TYPE TABLE OF type_data,
      t_file         TYPE TABLE OF zmmi0001,
      t_file_hvi     TYPE TABLE OF zmmi0001_hvi,
      w_file_hvi     TYPE zmmi0001_hvi,
      t_mchb         TYPE TABLE OF type_mchb,
      w_mchb         TYPE type_mchb,
      t_inob         TYPE TABLE OF type_inob,
      t_kssk         TYPE TABLE OF type_kssk,
      t_klah         TYPE TABLE OF type_klah,
      t_msg          TYPE TABLE OF type_msg,
      t_ksml         TYPE TABLE OF type_ksml,
      t_cabnt        TYPE TABLE OF type_cabnt,
      st_rmclm       TYPE type_rmclm,
      arquivo        TYPE c,
      l_erro         TYPE c,
      l_matnr        TYPE matnr,
      l_matkl        TYPE matkl,
      w_zppt0034     TYPE zppt0034,
      wa_calc        TYPE ty_calc,
      gw_safrafardos TYPE ztsafrafardos,
      tw_safrafardos TYPE TABLE OF ztsafrafardos.
*
TYPES: BEGIN OF ty_char,
         pos TYPE i,
         chr TYPE c.
TYPES: END   OF ty_char.
*
DATA: t_char   TYPE TABLE OF ty_char,
      l_chr1   TYPE c,
      l_chr2   TYPE c,
      l_chr3   TYPE c,
      l_dig1   TYPE i,
      l_dig2   TYPE i,
      l_dig3   TYPE i,
      l_ret2   TYPE char2,
      l_digito TYPE zdigfard,
      l_ok     TYPE c,
      l_import TYPE c.

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  s_charg  FOR ztsafrafardos-charg NO INTERVALS NO-EXTENSION OBLIGATORY,
  s_werks  FOR t001w-werks         NO INTERVALS NO-EXTENSION OBLIGATORY,
  s_matnr  FOR mara-matnr.
SELECTION-SCREEN END   OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE text-002.
PARAMETERS
  p_file TYPE file_name.
SELECTION-SCREEN END   OF BLOCK a2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-008.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 01.
SELECTION-SCREEN COMMENT 01(15) text-010 FOR FIELD rb_total.
PARAMETER: rb_total RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND abc.


SELECTION-SCREEN POSITION 41.
PARAMETER: rb_parc RADIOBUTTON GROUP g1 .
SELECTION-SCREEN COMMENT 23(13) text-009 FOR FIELD rb_parc.

*-CS2022000332-#78805-18.07.2022-JT-inicio
SELECTION-SCREEN POSITION 79.
PARAMETER: rb_terc RADIOBUTTON GROUP g1 .
SELECTION-SCREEN COMMENT 50(13) text-015 FOR FIELD rb_terc.
*-CS2022000332-#78805-18.07.2022-JT-inicio

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b3.

*----------------------------------------------------------------------*
*                          AT SELECTION-SCREEN                         *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
* Ajuda de Pesquisa Arquivo
  PERFORM z_busca_file.

*----------------------------------------------------------------------*
*                           Start of Selection                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  FREE t_msg.

* Upload File
  PERFORM z_upload_file.

  CHECK NOT t_file[] IS INITIAL.
  CHECK l_erro       IS INITIAL.

*-CS2022000332-#78805-19.07.2022-JT-inicio
  IF arquivo = c_hvi.
*---grava dados hvi
    PERFORM f_grava_dados.
  ELSE.
*-- Seleciona Dados
    PERFORM z_seleciona_dados.
*-- Processa Dados
    PERFORM z_processa_dados.
  ENDIF.
*-CS2022000332-#78805-19.07.2022-JT-fim

  IF l_import = abap_true.
    MESSAGE s024(sd) WITH text-100.
  ENDIF.

  IF NOT t_msg[] IS INITIAL.
    CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
      TABLES
        table    = t_msg
      EXCEPTIONS
        fb_error = 1
        OTHERS   = 2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_FILE                                             *
*&---------------------------------------------------------------------*
*                        Ajuda de Pesquisa Arquivo                     *
*----------------------------------------------------------------------*
FORM z_busca_file.

  DATA: vl_window TYPE string,
        vl_file   TYPE string,
        vl_rc     TYPE i,
        tl_file   TYPE filetable,
        sl_file   TYPE file_table.

  vl_file   = text-003.
  vl_window = text-004.

  REFRESH tl_file.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = vl_window
      file_filter             = vl_file
    CHANGING
      file_table              = tl_file
      rc                      = vl_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT tl_file[] IS INITIAL.
  READ TABLE tl_file INTO sl_file INDEX 1.
  p_file = sl_file.

ENDFORM.                    " Z_BUSCA_FILE

*&---------------------------------------------------------------------*
*&      Form  Z_UPLOAD_FILE                                            *
*&---------------------------------------------------------------------*
*                                Upload File                           *
*----------------------------------------------------------------------*
FORM z_upload_file.

  DATA vl_file TYPE string.

  REFRESH: t_data, t_file.
  FREE   : w_file_hvi.

  CHECK NOT p_file IS INITIAL.

  vl_file = p_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = vl_file
    CHANGING
      data_tab                = t_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE t_data INTO DATA(wa_heade) INDEX 1.

  DATA(heade)  = wa_heade(5).

*-CS2022000332-#78805-19.07.2022-JT-inicio
  PERFORM f_split_file_hvi USING wa_heade
                        CHANGING w_file_hvi.
  DATA(heade2) = w_file_hvi-peso.
*-CS2022000332-#78805-19.07.2022-JT-fim

  IF heade EQ heade_unicotton.
    arquivo = c_unicotton. "//'Imprimindo UNICOTTON'.
  ELSEIF heade EQ heade_kuhlmann AND heade2 = heade_hvi.
    arquivo = c_hvi. "//'Imprimindo KUHLMANN'.
  ELSEIF heade EQ heade_kuhlmann AND heade2 = abap_off.
    arquivo = c_kuhlmann. "//'Imprimindo KUHLMANN'.
  ELSE.
    MESSAGE i836 WITH 'Arquivo imcompativel para processamento!'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  DO 1 TIMES.
    DELETE t_data INDEX 1.
  ENDDO.

  IF t_data[] IS INITIAL.
    MESSAGE i836 WITH text-005.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Move Dados
  PERFORM z_move_dados.

ENDFORM.                    " Z_UPLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  F_GERA_DIGITO
*&---------------------------------------------------------------------*
FORM f_get_digito    USING p_lifnr
                           p_cod_gs1
                           p_safra
                  CHANGING p_digito.

  FREE: p_digito.

  SELECT SINGLE *
    FROM zppt0033
    INTO @DATA(w_zppt0033)
   WHERE lifnr   = @p_lifnr
     AND cod_gs1 = @p_cod_gs1
     AND safra   = @p_safra.

  IF sy-subrc = 0.
    p_digito = w_zppt0033-digito.
  ELSE.
    PERFORM f_gera_dig_fard CHANGING p_digito.

*---Atualiza tab digito
    w_zppt0033-mandt    = sy-mandt.
    w_zppt0033-lifnr    = p_lifnr.
    w_zppt0033-cod_gs1  = p_cod_gs1.
    w_zppt0033-safra    = p_safra.
    w_zppt0033-digito   = p_digito.
    MODIFY zppt0033  FROM w_zppt0033.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_GERA_DIGITO
*&---------------------------------------------------------------------*
FORM f_grava_dados.

  DATA: w_file     TYPE zmmi0001,
        w_zmmt0027 TYPE zmmt0027,
        w_zppt0002 TYPE zppt0002,
        w_zmmt0008 TYPE zmmt0008,
        l_charg    TYPE zmmt0027-charg.

  l_import = abap_false.

*-------------------------------------
* valida existencia do bloco
*-------------------------------------
  CALL FUNCTION 'ZSD_CHECA_CARGA_HVI'
    IMPORTING
      e_ok    = l_ok
    TABLES
      t_dados = t_file.

  LOOP AT t_file INTO w_file.

    PERFORM f_get_digito USING w_file-lifnr
                               w_file-cod_gs1
                               w_file-safra
                      CHANGING l_digito.

    l_charg = l_digito && w_file-fardos.

    IF l_ok = abap_false.
      SELECT SINGLE *
        FROM zmmt0027
        INTO w_zmmt0027
       WHERE charg = l_charg
         AND werks = w_file-werks
         AND matkl = w_file-matkl.
      CHECK sy-subrc <> 0.
    ENDIF.

    l_import = abap_true.

*--------------------------------
*-- monta tabela ZMMT0027
*--------------------------------
    CLEAR w_zmmt0027.

    w_zmmt0027-mandt          = sy-mandt.
    w_zmmt0027-charg          = l_charg.
    w_zmmt0027-werks          = w_file-werks.
    w_zmmt0027-matkl          = w_file-matkl.
    w_zmmt0027-matnr          = w_file-matnr.
    w_zmmt0027-safra          = w_file-safra.
    w_zmmt0027-menge          = w_file-peso.
    w_zmmt0027-lgort          = w_file-lgort.
    w_zmmt0027-normt          = w_file-normt.
    w_zmmt0027-budat          = sy-datum.
    w_zmmt0027-far_uhml       = w_file-uhml.
    w_zmmt0027-far_ui         = w_file-ui.
    w_zmmt0027-far_str        = w_file-str.
    w_zmmt0027-far_elg        = w_file-elg.
    w_zmmt0027-far_mic        = w_file-mic.
    w_zmmt0027-far_rd         = w_file-rd.
    w_zmmt0027-far_b          = w_file-plusb.
    w_zmmt0027-far_cg         = w_file-cg.
    w_zmmt0027-far_tcnt       = w_file-t_cnt.
    w_zmmt0027-far_tarea      = w_file-t_area.
    w_zmmt0027-far_leaf       = w_file-leaf.
    w_zmmt0027-far_mr         = w_file-mr.
    w_zmmt0027-far_sfiw       = w_file-sfi_w.
    w_zmmt0027-far_sci        = w_file-sci.
    w_zmmt0027-far_csp        = w_file-csp.
    w_zmmt0027-lifnr          = w_file-lifnr.
    w_zmmt0027-adquirido_terc = abap_true.
    MODIFY zmmt0027        FROM w_zmmt0027.

*--------------------------------
*-- monta tabela ZPPT0002
*--------------------------------
    CLEAR w_zppt0002.

    w_zppt0002-mandt          = sy-mandt.
    w_zppt0002-acharg         = w_zmmt0027-charg.
    w_zppt0002-werks          = w_zmmt0027-werks.
    w_zppt0002-matnr          = w_zmmt0027-matnr.
    w_zppt0002-menge          = w_zmmt0027-menge.
    w_zppt0002-budat          = w_zmmt0027-budat.
    w_zppt0002-bldat          = w_zmmt0027-budat.
    w_zppt0002-dt_fabricacao  = w_zmmt0027-budat.
    w_zppt0002-usnam          = sy-uname.
    w_zppt0002-cd_safra       = w_zmmt0027-safra.
    w_zppt0002-cd_sai         = w_file-cd_sai.
    w_zppt0002-peso_liquido   = w_zmmt0027-menge.
    w_zppt0002-cd_classificacao = w_file-normt.
    w_zppt0002-lgort          = w_file-lgort.
    MODIFY zppt0002        FROM w_zppt0002.

*--------------------------------
*-- monta tabela ZMMT0008
*--------------------------------
    CLEAR w_zmmt0008.

    w_zmmt0008-mandt          = sy-mandt.
    w_zmmt0008-werks          = w_zmmt0027-werks.
    w_zmmt0008-lgort          = w_file-lgort.
    w_zmmt0008-charg          = w_zmmt0027-charg.
    w_zmmt0008-menge          = w_zmmt0027-menge.
    MODIFY zmmt0008        FROM w_zmmt0008.

    COMMIT WORK AND WAIT.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_DADOS                                             *
*&---------------------------------------------------------------------*
*                               Move Dados                             *
*----------------------------------------------------------------------*
FORM f_grava_tabela_digitos USING sl_data TYPE type_data.

  DATA(l_lifnr) = |{ sl_data-campo+217(10) ALPHA = IN }|.
  DATA(l_codgsi) = sl_data-campo+13(9).



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_DADOS                                             *
*&---------------------------------------------------------------------*
*                               Move Dados                             *
*----------------------------------------------------------------------*
FORM f_valida_dados    USING sl_data TYPE zmmi0001_hvi
                    CHANGING p_matnr
                             p_matkl
                             p_erro.

  FREE: p_erro,
        p_matnr.

  DATA(l_safra2) = sl_data-safra+5(4).

  IF s_charg-low <> l_safra2.
    p_erro = abap_true.
    DATA(l_safra) = l_safra2.
    MESSAGE i024(sd) WITH 'Safra do filtro' s_charg-low 'diferente da Safra do Arquivo' l_safra.
    EXIT.
  ENDIF.

  DATA(l_lifnr) = |{ sl_data-cd_fornecedor(10) ALPHA = IN }|.

  SELECT SINGLE lifnr
    INTO @DATA(_lifnr)
    FROM lfa1
   WHERE lifnr = @l_lifnr.

  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE i024(sd) WITH 'Código do Produtor' l_lifnr 'não localizado como' 'fornecedor no SAP.'.
    EXIT.
  ENDIF.

  DATA(l_normt) = sl_data-tipo.

  SELECT matnr, normt, matkl
    INTO @DATA(_mara)
      UP TO 1 ROWS
    FROM mara
   WHERE normt = @l_normt
     AND mtart = 'ZHAW'.
  ENDSELECT.

  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE i024(sd) WITH 'Não localizado nenhum código de Material' 'para o Tipo' l_normt.
    EXIT.
  ENDIF.

  SELECT werks
    INTO @DATA(_werks)
      UP TO 1 ROWS
    FROM mard
   WHERE matnr = @_mara-matnr
     AND werks = @s_werks-low.
  ENDSELECT.

  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE i024(sd) WITH 'Não localizado nenhum código de Material' 'para o Tipo' l_normt.
    EXIT.
  ENDIF.

  p_matnr = _mara-matnr.
  p_matkl = _mara-matkl.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_DADOS                                             *
*&---------------------------------------------------------------------*
*                               Move Dados                             *
*----------------------------------------------------------------------*
FORM f_split_file_hvi USING p_file
                   CHANGING p_file_hvi.

  FIELD-SYMBOLS: <f_file> TYPE any.

  DATA: w_string TYPE string.

  SPLIT p_file AT cl_abap_char_utilities=>horizontal_tab
             INTO TABLE t_file_hvi.

  LOOP AT t_file_hvi        INTO w_string.
    ASSIGN COMPONENT sy-tabix OF STRUCTURE w_file_hvi TO <f_file>.
    <f_file> = w_string.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_DADOS                                             *
*&---------------------------------------------------------------------*
*                               Move Dados                             *
*----------------------------------------------------------------------*
FORM z_move_dados.
  DATA: sl_data             TYPE type_data,
        sl_file             TYPE zmmi0001,
        wa_setleaf          TYPE setleaf,
        wl_comp_far         TYPE c LENGTH 15,
        vlinesl             TYPE sy-tabix,
        vlinest             TYPE sy-tabix,
        w_zmmt0027          TYPE zmmt0027,
        wl_setnr(30),
        vg_achou(1),
        it_texto            TYPE TABLE OF string,
        it_uhm              TYPE TABLE OF string,
        b                   TYPE string,
        loc_set_lines_basic LIKE rgsbv OCCURS 0 WITH HEADER LINE.

  CASE arquivo.
    WHEN c_unicotton.
      DATA(equipment) = t_data[ 1 ]-campo+1(12).
    WHEN c_kuhlmann.
      equipment = t_data[ 1 ]-campo+10(12).
  ENDCASE.

*  SELECT SINGLE *
*    FROM ZTSAFRAFARDOS
*    INTO GW_SAFRAFARDOS
*   WHERE CHARG      EQ S_CHARG-LOW
*     AND WERKS_FROM EQ S_WERKS-LOW
*      AND MATNR      EQ EQUIPMENT.

  IF arquivo <> c_hvi.  "CS2022000332-#78805-19.07.2022-JT-inicio
    SELECT *
     FROM ztsafrafardos
     INTO TABLE tw_safrafardos
    WHERE charg      EQ s_charg-low
      AND werks_to   EQ s_werks-low
      AND matnr      EQ equipment.

    IF tw_safrafardos[] IS INITIAL.
      MESSAGE text-007 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  REFRESH t_file.
  FREE: l_erro.

  IF arquivo = c_hvi.
    DELETE t_data WHERE campo(10) = abap_off.
  ENDIF.

  LOOP AT t_data INTO sl_data.

    CLEAR w_file_hvi.

    IF sl_data-campo(01) EQ '-'.
      EXIT.
    ENDIF.

*-CS2022000332-#78805-19.07.2022-JT-inicio
    IF arquivo = c_hvi.
      PERFORM f_split_file_hvi USING sl_data
                            CHANGING w_file_hvi.
    ENDIF.
*-CS2022000332-#78805-19.07.2022-JT-fim

    APPEND INITIAL LINE TO tw_safrafardos.

    CLEAR vg_achou.
    LOOP AT tw_safrafardos INTO gw_safrafardos. "Testa se o fardo HVI é de uma das origem que são beneficiadas

      CASE arquivo.
        WHEN c_unicotton.
          CONCATENATE gw_safrafardos-werks_key
                      sl_data-campo+13(07) INTO sl_file-fardos.
          SELECT SINGLE matnr werks lgort charg
              FROM mchb
                INTO w_mchb
                    WHERE matnr IN s_matnr
                      AND werks EQ gw_safrafardos-werks_to
                      AND charg EQ sl_file-fardos.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.

        WHEN c_kuhlmann.
          CONCATENATE gw_safrafardos-werks_key
                      sl_data-campo+22(07) INTO sl_file-fardos.
          SELECT SINGLE matnr werks lgort charg
              FROM mchb
                INTO w_mchb
                    WHERE matnr IN s_matnr
                      AND werks EQ gw_safrafardos-werks_to
                      AND charg EQ sl_file-fardos.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.

*-CS2022000332-#78805-19.07.2022-JT-inicio
        WHEN c_hvi.
          PERFORM f_valida_dados    USING w_file_hvi
                                 CHANGING l_matnr
                                          l_matkl
                                          l_erro.
          IF l_erro = abap_true.
            EXIT.
          ENDIF.
*-CS2022000332-#78805-19.07.2022-JT-fim

      ENDCASE.

*      SELECT SINGLE matnr werks lgort charg
*          FROM mchb
*            INTO w_mchb
*                WHERE matnr IN s_matnr
*                  AND werks EQ gw_safrafardos-werks_to
*                  AND charg EQ sl_file-fardos.
*
*      IF sy-subrc NE 0.
*        CONTINUE.
*      ENDIF.

      DATA(fard) = sl_file-fardos.
      IF rb_parc IS INITIAL.

        CASE arquivo.
          WHEN c_unicotton.

            REPLACE ALL OCCURRENCES OF ',' IN sl_data-campo WITH '.'.

            SPLIT sl_data-campo AT ';' INTO TABLE it_texto.

            sl_file-mic      = it_texto[ 4  ].     CONDENSE sl_file-mic   .  "1   MIC
            sl_file-uhml     = it_texto[ 6  ].     CONDENSE sl_file-uhml  .  "2   POL
            sl_file-str      = it_texto[ 11 ].     CONDENSE sl_file-str   .  "4   STR
            sl_file-ui       = it_texto[ 9  ].     CONDENSE sl_file-ui    .  "5   UNF
            sl_file-elg      = it_texto[ 12 ].     CONDENSE sl_file-elg   .  "6   ELG
            sl_file-mr       = it_texto[ 5  ].     CONDENSE sl_file-mr    .  "7   MAT
            sl_file-mr = sl_file-mr / 100.
            sl_file-rd       = it_texto[ 13 ].     CONDENSE sl_file-rd    .  "8   RD
            sl_file-plusb    = it_texto[ 14 ].     CONDENSE sl_file-plusb .  "9   B
            sl_file-cg       = it_texto[ 15 ].     CONDENSE sl_file-cg    .  "10  CG
            REPLACE ALL OCCURRENCES OF '"' IN sl_file-cg WITH ''.
            sl_file-leaf     = it_texto[ 18 ].     CONDENSE sl_file-leaf  .  "11  LEAF
            sl_file-t_area   = it_texto[ 17 ].     CONDENSE sl_file-t_area.  "12  AREA
            sl_file-t_cnt    = it_texto[ 16 ].     CONDENSE sl_file-t_cnt .  "13  COUNT
            sl_file-sfi_w    = it_texto[ 10 ].     CONDENSE sl_file-sfi_w .  "14  SFI
            sl_file-sci      = it_texto[ 3  ].     CONDENSE sl_file-sci   .  "15  SCI

            wa_calc-c_str   = '8.24'.
            wa_calc-c_uhm   = '33.5'.
            wa_calc-c_ui    = '15.2'.
            wa_calc-c_rd    = '14.84'.
            wa_calc-c_b     = '27.87'.
            wa_calc-c_trash = '5.02'.
            wa_calc-c_mic   = '97.8'.

            wa_calc-c_fixo  = '741.08'.

            wa_calc-v_str   = sl_file-str.

*            SPLIT sl_file-uhml AT '/' INTO TABLE it_uhm.
*
*            wa_calc-v_uhm_1   = it_uhm[ 1 ].
*            wa_calc-v_uhm_2   = it_uhm[ 2 ].

*            wa_calc-v_uhm = wa_calc-v_uhm_1 / wa_calc-v_uhm_2.

            wa_calc-v_uhm   = sl_file-uhml.
            wa_calc-v_ui    = sl_file-ui.
            wa_calc-v_rd    = sl_file-rd.
            wa_calc-v_b     = sl_file-plusb.
            wa_calc-v_trash = sl_file-leaf.
            wa_calc-v_mic   = sl_file-mic.

            wa_calc-r_str   =  wa_calc-c_str   * wa_calc-v_str.
            wa_calc-r_uhm   =  wa_calc-c_uhm   * wa_calc-v_uhm.
            wa_calc-r_ui    =  wa_calc-c_ui    * wa_calc-v_ui.
            wa_calc-r_rd    =  wa_calc-c_rd    * wa_calc-v_rd.
            wa_calc-r_b     =  wa_calc-c_b     * wa_calc-v_b.
            wa_calc-r_trash =  wa_calc-c_trash * wa_calc-v_trash.
            wa_calc-r_mic   =  wa_calc-c_mic   * wa_calc-v_mic.

            wa_calc-f_csp = ( ( wa_calc-r_str + wa_calc-r_uhm + wa_calc-r_ui + wa_calc-r_rd ) - ( wa_calc-r_b + wa_calc-r_trash + wa_calc-r_mic ) ) - wa_calc-c_fixo.

            sl_file-csp = wa_calc-f_csp. CONDENSE sl_file-csp   .  "16  CSP

          WHEN c_kuhlmann.

            sl_file-mic      = sl_data-campo+52(5).     CONDENSE sl_file-mic   .  "1   MIC
            sl_file-uhml     = sl_data-campo+57(4).     CONDENSE sl_file-uhml  .  "2   POL - BUG 64009 / IR068363 - AOENNING. 09/08/2021 - Almentar casa de 5 para 6.
            sl_file-str      = sl_data-campo+69(5).     CONDENSE sl_file-str   .  "4   STR
            sl_file-ui       = sl_data-campo+74(5).     CONDENSE sl_file-ui    .  "5   UNF
            sl_file-elg      = sl_data-campo+79(4).     CONDENSE sl_file-elg   .  "6   ELG
            sl_file-mr       = sl_data-campo+83(5).     CONDENSE sl_file-mr    .  "7   MAT
            sl_file-rd       = sl_data-campo+88(5).     CONDENSE sl_file-rd    .  "8   RD
            sl_file-plusb    = sl_data-campo+94(4).     CONDENSE sl_file-plusb .  "9   B
            sl_file-cg       = sl_data-campo+98(4).     CONDENSE sl_file-cg    .  "10  CG
            sl_file-leaf     = sl_data-campo+103(5).    CONDENSE sl_file-leaf  .  "11  LEAF
            sl_file-t_area   = sl_data-campo+108(5).    CONDENSE sl_file-t_area.  "12  AREA
            sl_file-t_cnt    = sl_data-campo+114(5).    CONDENSE sl_file-t_cnt .  "13  COUNT
            sl_file-sfi_w    = sl_data-campo+119(4).    CONDENSE sl_file-sfi_w .  "14  SFI
            sl_file-sci      = sl_data-campo+124(4).    CONDENSE sl_file-sci   .  "15  SCI
            sl_file-csp      = sl_data-campo+128(5).    CONDENSE sl_file-csp   .  "16  CSP

*-CS2022000332-#78805-19.07.2022-JT-inicio
          WHEN c_hvi.

            sl_file-fardos   = w_file_hvi-fardo+12(7).  CONDENSE sl_file-fardos.
            sl_file-lifnr    = |{ w_file_hvi-cd_fornecedor ALPHA = IN }|.  CONDENSE sl_file-lifnr.
            sl_file-cod_gs1  = w_file_hvi-fardo+3(9).   CONDENSE sl_file-cod_gs1.
            sl_file-werks    = s_werks-low.             CONDENSE sl_file-werks.
            sl_file-matnr    = l_matnr.                 CONDENSE sl_file-matnr.
            sl_file-matkl    = l_matkl.                 CONDENSE sl_file-matkl.
            sl_file-safra    = w_file_hvi-safra+5(4).   CONDENSE sl_file-safra.
            sl_file-normt    = w_file_hvi-tipo.         CONDENSE sl_file-normt.
            sl_file-lgort    = w_file_hvi-bloco.        CONDENSE sl_file-lgort.
            sl_file-cd_sai   = w_file_hvi-fardo.        CONDENSE sl_file-cd_sai.
            sl_file-peso     = w_file_hvi-peso.         CONDENSE sl_file-peso.
            sl_file-mic      = w_file_hvi-mic.          CONDENSE sl_file-mic   .  "1   MIC
            sl_file-uhml     = w_file_hvi-pol.          CONDENSE sl_file-uhml  .  "2   POL - BUG 64009 / IR068363 - AOENNING. 09/08/2021 - Almentar casa de 5 para 6.
            sl_file-str      = w_file_hvi-str.          CONDENSE sl_file-str   .  "4   STR
            sl_file-ui       = w_file_hvi-unf.          CONDENSE sl_file-ui    .  "5   UNF
            sl_file-elg      = w_file_hvi-elg.          CONDENSE sl_file-elg   .  "6   ELG
            sl_file-mr       = w_file_hvi-mat.          CONDENSE sl_file-mr    .  "7   MAT
            sl_file-rd       = w_file_hvi-rd.           CONDENSE sl_file-rd    .  "8   RD
            sl_file-plusb    = w_file_hvi-b.            CONDENSE sl_file-plusb .  "9   B
            sl_file-cg       = w_file_hvi-cg.           CONDENSE sl_file-cg    .  "10  CG
            sl_file-leaf     = w_file_hvi-leaf.         CONDENSE sl_file-leaf  .  "11  LEAF
            sl_file-t_area   = w_file_hvi-area.         CONDENSE sl_file-t_area.  "12  AREA
            sl_file-t_cnt    = w_file_hvi-counter.      CONDENSE sl_file-t_cnt .  "13  COUNT
            sl_file-sfi_w    = w_file_hvi-sfi.          CONDENSE sl_file-sfi_w .  "14  SFI
            sl_file-sci      = w_file_hvi-sci.          CONDENSE sl_file-sci   .  "15  SCI
            sl_file-csp      = w_file_hvi-csp.          CONDENSE sl_file-csp   .  "16  CSP

            PERFORM z_trca_p_v CHANGING: sl_file-mic   ,
                                         sl_file-uhml  ,
                                         sl_file-str   ,
                                         sl_file-ui    ,
                                         sl_file-elg   ,
                                         sl_file-mr    ,
                                         sl_file-rd    ,
                                         sl_file-plusb ,
                                         sl_file-cg    ,
                                         sl_file-leaf  ,
                                         sl_file-t_area,
                                         sl_file-t_cnt ,
                                         sl_file-sfi_w ,
                                         sl_file-sci   ,
                                         sl_file-csp   ,
                                         sl_file-peso  .

            IF sl_file-csp IS INITIAL.
              wa_calc-c_str   = '8.24'.
              wa_calc-c_uhm   = '33.5'.
              wa_calc-c_ui    = '15.2'.
              wa_calc-c_rd    = '14.84'.
              wa_calc-c_b     = '27.87'.
              wa_calc-c_trash = '5.02'.
              wa_calc-c_mic   = '97.8'.
              wa_calc-c_fixo  = '741.08'.
              wa_calc-v_str   = sl_file-str.
              wa_calc-v_uhm   = sl_file-uhml.
              wa_calc-v_ui    = sl_file-ui.
              wa_calc-v_rd    = sl_file-rd.
              wa_calc-v_b     = sl_file-plusb.
              wa_calc-v_trash = sl_file-leaf.
              wa_calc-v_mic   = sl_file-mic.
              wa_calc-r_str   =  wa_calc-c_str   * wa_calc-v_str.
              wa_calc-r_uhm   =  wa_calc-c_uhm   * wa_calc-v_uhm.
              wa_calc-r_ui    =  wa_calc-c_ui    * wa_calc-v_ui.
              wa_calc-r_rd    =  wa_calc-c_rd    * wa_calc-v_rd.
              wa_calc-r_b     =  wa_calc-c_b     * wa_calc-v_b.
              wa_calc-r_trash =  wa_calc-c_trash * wa_calc-v_trash.
              wa_calc-r_mic   =  wa_calc-c_mic   * wa_calc-v_mic.
              wa_calc-f_csp = ( ( wa_calc-r_str + wa_calc-r_uhm + wa_calc-r_ui + wa_calc-r_rd ) - ( wa_calc-r_b + wa_calc-r_trash + wa_calc-r_mic ) ) - wa_calc-c_fixo.
              sl_file-csp = wa_calc-f_csp. CONDENSE sl_file-csp   .  "16  CSP
            ENDIF.
*-CS2022000332-#78805-19.07.2022-JT-fim

        ENDCASE.

      ELSE.

        SELECT SINGLE *
        FROM zmmt0027
        INTO CORRESPONDING FIELDS OF w_zmmt0027
          WHERE werks EQ  s_werks-low
            AND charg EQ sl_file-fardos.

        IF w_zmmt0027 IS NOT INITIAL.
          sl_file = VALUE #(
                           fardos   = fard
                           mic      = w_zmmt0027-far_mic
                           uhml     = w_zmmt0027-far_uhml
                           str      = w_zmmt0027-far_str
                           ui       = w_zmmt0027-far_ui
                           elg      = w_zmmt0027-far_elg
                           mr       = w_zmmt0027-far_mr
                           rd       = w_zmmt0027-far_rd
                           plusb    = w_zmmt0027-far_b
                           cg       = w_zmmt0027-far_cg
                           leaf     = w_zmmt0027-far_leaf
                           t_area   = w_zmmt0027-far_tarea
                           t_cnt    = w_zmmt0027-far_tcnt
                           sfi_w    = w_zmmt0027-far_sfiw
                           sci      = w_zmmt0027-far_sci
                           csp      = w_zmmt0027-far_csp   ).
        ELSE.
          PERFORM z_retorna_msg_2 USING text-011
                                  sl_file-fardos.
          vg_achou = 'Y'.
          EXIT.
        ENDIF.

        DATA(sc) = sl_data-campo+09(1).
        SPLIT sl_data-campo AT sc INTO: DATA(os) DATA(fardo)
                                        DATA(date) DATA(lote) DATA(mic) DATA(uhml)
                                        DATA(len) DATA(str) DATA(ui) DATA(elg) DATA(mr)
                                        DATA(rd) DATA(plusb) DATA(cg)
                                        DATA(leaf) DATA(t_area) DATA(t_cnt)
                                        DATA(sfi_w) DATA(sci) DATA(csp).
        CONDENSE:
        mic, uhml, len, str, ui, elg, mr, rd, plusb,
        cg, leaf, t_area, t_cnt, sfi_w, sci, csp.

        IF sc IS NOT INITIAL.
          REPLACE ALL OCCURRENCES OF:
                sc IN mic      WITH '',
                sc IN uhml     WITH '',
                sc IN len      WITH '',
                sc IN str      WITH '',
                sc IN ui       WITH '',
                sc IN elg      WITH '',
                sc IN mr       WITH '',
                sc IN rd       WITH '',
                sc IN plusb    WITH '',
                sc IN cg       WITH '',
                sc IN leaf     WITH '',
                sc IN t_area   WITH '',
                sc IN t_cnt    WITH '',
                sc IN sfi_w    WITH '',
                sc IN sci      WITH '',
                sc IN csp      WITH ''.
        ENDIF.

        IF mic IS NOT INITIAL.
          sl_file-mic = mic.
        ENDIF.

        IF uhml IS NOT INITIAL.
          sl_file-uhml  = uhml.
        ENDIF.

        IF str IS NOT INITIAL.
          sl_file-str = str.
        ENDIF.

        IF ui IS NOT INITIAL.
          sl_file-ui  = ui.
        ENDIF.

        IF elg IS NOT INITIAL.
          sl_file-elg = elg.
        ENDIF.

        IF mr IS NOT INITIAL.
          sl_file-mr  = mr.
        ENDIF.

        IF rd IS NOT INITIAL.
          sl_file-rd  = rd.
        ENDIF.

        IF plusb IS NOT INITIAL.
          sl_file-plusb = plusb.
        ENDIF.

        IF cg IS NOT INITIAL.
          sl_file-cg = cg.
        ENDIF.

        IF leaf IS NOT INITIAL.
          sl_file-leaf = leaf.
        ENDIF.

        IF t_area IS NOT INITIAL.
          sl_file-t_area = t_area.
        ENDIF.

        IF t_cnt IS NOT INITIAL.
          sl_file-t_cnt = t_cnt.
        ENDIF.

        IF sfi_w IS NOT INITIAL.
          sl_file-sfi_w = sfi_w.
        ENDIF.

        IF sci IS NOT INITIAL.
          sl_file-sci  = sci.
        ENDIF.

        IF csp IS NOT INITIAL.
          sl_file-csp  = csp.
        ENDIF.
      ENDIF.

*   Troca , por .
      PERFORM z_trca_p_v CHANGING: sl_file-mic   ,
                                   sl_file-uhml  ,
                                   sl_file-str   ,
                                   sl_file-ui    ,
                                   sl_file-elg   ,
                                   sl_file-mr    ,
                                   sl_file-rd    ,
                                   sl_file-plusb ,
                                   sl_file-cg    ,
                                   sl_file-leaf  ,
                                   sl_file-t_area,
                                   sl_file-t_cnt ,
                                   sl_file-sfi_w ,
                                   sl_file-sci   ,
                                   sl_file-csp   .

      APPEND sl_file TO t_file.

      vg_achou = abap_true.
      EXIT.
    ENDLOOP.

    IF l_erro = abap_true.
      EXIT.
    ENDIF.

    IF vg_achou EQ abap_false.
*   Retorna MSG
      PERFORM z_retorna_msg_2 USING text-006
                              sl_file-fardos.
    ENDIF.
    CLEAR: sl_data, sl_file, mic, uhml, len, str, ui, elg, mr, rd, plusb, cg, leaf, t_area, t_cnt, sfi_w, sci, csp, sc, fard.

  ENDLOOP.

*  REFRESH T_MCHB.
*
*  IF T_FILE[] IS NOT INITIAL.
*    SELECT MATNR WERKS LGORT CHARG
*           FROM MCHB
*             INTO TABLE T_MCHB
*             FOR ALL ENTRIES IN T_FILE
*                 WHERE MATNR IN S_MATNR
*                   AND WERKS EQ GW_SAFRAFARDOS-WERKS_TO
*                   AND CHARG EQ T_FILE-FARDOS.
*  ENDIF.
*
*  IF  T_MCHB[] IS NOT INITIAL. "achou a filial que foi beneficiado
*    DESCRIBE TABLE T_MCHB LINES VLINESL.
*    DESCRIBE TABLE T_FILE LINES VLINEST.
*    IF VLINEST GT 0.
*      IF ( VLINESL / VLINEST ) LT ( 5 / 100 ). "pelo menos 5% do lotes encontrados
*        REFRESH T_FILE.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  REFRESH T_MCHB.

  CHECK t_file[] IS INITIAL AND l_erro = abap_false.
  MESSAGE i836 WITH text-005.
  LEAVE LIST-PROCESSING.

ENDFORM.                    " Z_MOVE_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                            Seleciona Dados                           *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona MCHB
  PERFORM: z_seleciona_mchb,
* Seleciona Dados Características
           z_seleciona_carac.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                             Processa Dados                           *
*----------------------------------------------------------------------*
FORM z_processa_dados.
  DATA: sl_file TYPE zmmi0001,
        sl_mchb TYPE type_mchb.

  SORT: t_inob  BY objek ASCENDING,
        t_kssk  BY objek ASCENDING,
        t_klah  BY clint ASCENDING,
        t_cabnt BY atnam ASCENDING.

  LOOP AT t_file INTO sl_file.
    LOOP AT t_mchb INTO sl_mchb WHERE charg EQ sl_file-fardos.

*     Executa Bapi MSC2N
      PERFORM z_executa_msc2n USING sl_file
                                    sl_mchb.

      CLEAR sl_mchb.
    ENDLOOP.

    CLEAR sl_file.
  ENDLOOP.

ENDFORM.                    " Z_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTA_MSC2N                                          *
*&---------------------------------------------------------------------*
*                         Executa Bapi MSC2N                           *
*----------------------------------------------------------------------*
FORM z_executa_msc2n USING p_file TYPE zmmi0001
                           p_mchb TYPE type_mchb.

  DATA: vl_num   TYPE bapi1003_key-classnum,
        vl_type  TYPE bapi1003_key-classtype,
        vl_table TYPE bapi1003_key-objecttable,
        vl_key   TYPE bapi1003_key-object,
        sl_keys  TYPE bapi1003_object_keys,
        sl_inob  TYPE type_inob,
        sl_kssk  TYPE type_kssk,
        sl_klah  TYPE type_klah,
        tl_table TYPE TABLE OF bapi1003_object_keys,
        tl_ret   TYPE TABLE OF bapiret2,
        tl_num   TYPE TABLE OF bapi1003_alloc_values_num,
        tl_char  TYPE TABLE OF bapi1003_alloc_values_char,
        tl_curr  TYPE TABLE OF bapi1003_alloc_values_curr.

  REFRESH: tl_table, tl_ret.
  CLEAR: sl_inob, sl_kssk, sl_klah.

  sl_keys-key_field = 'MATNR'.
  sl_keys-value_int = p_mchb-matnr.
  APPEND sl_keys TO tl_table.

  sl_keys-key_field = 'CHARG'.
  sl_keys-value_int = p_mchb-charg.
  APPEND sl_keys TO tl_table.

*  sl_keys-key_field = 'WERKS'.
*  sl_keys-value_int = p_mchb-werks.
*  APPEND sl_keys TO tl_table.

  vl_table = 'MCH1'.

  CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
    EXPORTING
      objecttable    = vl_table
    IMPORTING
      objectkey_conc = vl_key
    TABLES
      objectkeytable = tl_table
      return         = tl_ret.

  CHECK sy-subrc IS INITIAL.

  READ TABLE t_inob INTO sl_inob WITH KEY objek = p_mchb-objek BINARY SEARCH.

  READ TABLE t_kssk INTO sl_kssk WITH KEY objek = sl_inob-objekk BINARY SEARCH.

  READ TABLE t_klah INTO sl_klah WITH KEY clint = sl_kssk-clint BINARY SEARCH.

  vl_num  = sl_klah-class.
  vl_type = st_rmclm-klart.

* Preenche Características
  PERFORM z_preenche_carac TABLES tl_char
                                  tl_num
                            USING p_file
                                  vl_key
                                  vl_table
                                  vl_num
                                  vl_type.

  CALL FUNCTION 'BAPI_OBJCL_CHANGE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      objectkey          = vl_key
      objecttable        = vl_table
      classnum           = vl_num
      classtype          = vl_type
    TABLES
      allocvaluesnumnew  = tl_num
      allocvaluescharnew = tl_char
      allocvaluescurrnew = tl_curr
      return             = tl_ret.

  DELETE tl_ret WHERE ( type NE 'E' AND type NE 'S' ).
* Retorna MSG
  PERFORM z_retorna_msg TABLES tl_ret
                         USING p_mchb-matnr
                               p_mchb-charg
                               p_mchb-werks.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.                    " Z_EXECUTA_MSC2N

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CARAC                                         *
*&---------------------------------------------------------------------*
*                      Preenche Características                        *
*----------------------------------------------------------------------*
FORM z_preenche_carac TABLES p_char  STRUCTURE bapi1003_alloc_values_char
                             p_tnum  STRUCTURE bapi1003_alloc_values_num
                       USING p_file  TYPE zmmi0001
                             p_key   TYPE bapi1003_key-object
                             p_table TYPE bapi1003_key-objecttable
                             p_num   TYPE bapi1003_key-classnum
                             p_type  TYPE bapi1003_key-classtype.

  DATA: tl_cat   TYPE lvc_t_fcat,
        sl_cat   TYPE lvc_s_fcat,
        vl_campo TYPE char30,
        sl_char  TYPE bapi1003_alloc_values_char,
        sl_num   TYPE bapi1003_alloc_values_num,
        sl_aux   TYPE bapi1003_alloc_values_char,
        sl_cabnt TYPE type_cabnt,
        tl_aux   TYPE TABLE OF bapi1003_alloc_values_char,
        tl_num   TYPE TABLE OF bapi1003_alloc_values_num,
        tl_char  TYPE TABLE OF bapi1003_alloc_values_char,
        tl_curr  TYPE TABLE OF bapi1003_alloc_values_curr,
        tl_ret   TYPE TABLE OF bapiret2,
        campo    TYPE char30.

  FIELD-SYMBOLS <campo> TYPE char11. "aoenning

  REFRESH: p_char, p_tnum.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      objectkey       = p_key
      objecttable     = p_table
      classnum        = p_num
      classtype       = p_type
    TABLES
      allocvaluesnum  = tl_num
      allocvalueschar = tl_char
      allocvaluescurr = tl_curr
      return          = tl_ret.

  SORT: tl_num  BY charact ASCENDING,
        tl_char BY charact ASCENDING.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMI0001'
    CHANGING
      ct_fieldcat            = tl_cat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DELETE tl_cat INDEX 1.

  LOOP AT tl_cat INTO sl_cat.
    CLEAR: vl_campo, sl_aux, campo.

*-CS2022000332-#78805-19.07.2022-JT-inicio
    CHECK sl_cat-fieldname <> 'LIFNR'   AND
          sl_cat-fieldname <> 'COD_GS1' AND
          sl_cat-fieldname <> 'SAFRA'   AND
          sl_cat-fieldname <> 'NORMT'   AND
          sl_cat-fieldname <> 'WERKS'   AND
          sl_cat-fieldname <> 'MATNR'   AND
          sl_cat-fieldname <> 'MATKL'   AND
          sl_cat-fieldname <> 'LGORT'   AND
          sl_cat-fieldname <> 'PESO'    AND
          sl_cat-fieldname <> 'CD_SAI'.
*-CS2022000332-#78805-19.07.2022-JT-fim

    CONCATENATE 'P_FILE' sl_cat-fieldname INTO vl_campo SEPARATED BY '-'.

*    campo = CONV #( vl_campo ).
    ASSIGN (vl_campo) TO <campo>.

*    CHECK campo is INITIAL.
    CHECK <campo> IS ASSIGNED.

    CASE sl_cat-fieldname.
      WHEN 'PLUSB'.
        sl_cat-fieldname = '+B'.
      WHEN 'T_CNT'.
        sl_cat-fieldname = 'T.CNT'.
      WHEN 'T_AREA'.
        sl_cat-fieldname = 'T.AREA'.
      WHEN 'SFI_W'.
        sl_cat-fieldname = 'SFI(W)'.
    ENDCASE.

    sl_aux-charact_descr  = sl_cat-fieldname.
    sl_aux-value_char     = <campo>.
*    sl_aux-value_char     = campo.
    APPEND sl_aux TO tl_aux.

    CLEAR sl_cat.
    UNASSIGN <campo>.
  ENDLOOP.

  SORT tl_aux BY charact_descr ASCENDING.

  LOOP AT t_cabnt INTO sl_cabnt.
    CLEAR: p_char, p_tnum.

    READ TABLE tl_aux INTO sl_aux WITH KEY charact_descr = sl_cabnt-atbez BINARY SEARCH.
    IF NOT sl_aux-value_char IS INITIAL.
      CASE sl_cabnt-atfor.
        WHEN 'CHAR'.
          p_char-charact     = sl_cabnt-atnam.
          p_char-value_char  = sl_aux-value_char.
          APPEND p_char.
        WHEN 'NUM'.
          p_tnum-charact     = sl_cabnt-atnam.
          p_tnum-value_from  = sl_aux-value_char.
          APPEND p_tnum.
      ENDCASE.
    ELSE.
      CASE sl_cabnt-atbez.
*        WHEN 'Safra'.
*          READ TABLE tl_num INTO sl_num WITH KEY charact = sl_cabnt-atnam BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*            p_tnum-charact     = sl_cabnt-atnam.
*            p_tnum-value_from  = sl_num-value_from.
*            APPEND p_tnum.
*          ENDIF.
        WHEN 'Variedade' OR 'Talhao'
          OR 'Safra' OR 'Periodo'.    "ADD - 21.06.2013

          READ TABLE tl_char INTO sl_char WITH KEY charact = sl_cabnt-atnam BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            p_char-charact     = sl_cabnt-atnam.
            p_char-value_char  = sl_char-value_char.
            APPEND p_char.
          ENDIF.
      ENDCASE.
    ENDIF.

    CLEAR: sl_cabnt, sl_aux, sl_num, sl_char.
  ENDLOOP.

*  LOOP AT tl_num INTO sl_num.
*    CLEAR p_tnum.
*
*    READ TABLE tl_aux INTO sl_aux WITH KEY charact_descr = sl_num-charact_descr BINARY SEARCH.
*
*    CHECK sy-subrc IS INITIAL.
*
*    p_tnum-charact     = sl_num-charact.
*    p_tnum-value_from  = sl_aux-value_char.
*    APPEND p_tnum.
*
*    CLEAR sl_num.
*  ENDLOOP.
*
*  LOOP AT tl_char INTO sl_char.
*    CLEAR p_char.
*
*    READ TABLE tl_aux INTO sl_aux WITH KEY charact_descr = sl_char-charact_descr BINARY SEARCH.
*
*    CHECK sy-subrc IS INITIAL.
*
*    p_char-charact     = sl_char-charact.
*    p_char-value_char  = sl_aux-value_char.
*    APPEND p_char.
*
*    CLEAR sl_num.
*  ENDLOOP.

ENDFORM.                    " Z_PREENCHE_CARAC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MCHB                                         *
*&---------------------------------------------------------------------*
*                            Seleciona MCHB                            *
*----------------------------------------------------------------------*
FORM z_seleciona_mchb.

  DATA tl_file TYPE TABLE OF zmmi0001.

  tl_file[] = t_file[].
  SORT tl_file BY fardos ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_file COMPARING fardos.

  SELECT matnr werks lgort charg
  FROM mchb
    INTO TABLE t_mchb
    FOR ALL ENTRIES IN tl_file
  WHERE matnr IN s_matnr
    AND werks IN s_werks
    AND charg EQ tl_file-fardos.

  SORT t_mchb BY charg ASCENDING
                 matnr ASCENDING.

  DELETE ADJACENT DUPLICATES FROM t_mchb COMPARING charg matnr.

  IF t_mchb[] IS INITIAL.
    MESSAGE i836 WITH text-005.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Move Matnr p/ Objek
  PERFORM z_move_matnr_objek.

* Verifica Lote
  PERFORM z_verifica_lote.

ENDFORM.                    " Z_SELECIONA_MCHB

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_MATNR_OBJEK                                       *
*&---------------------------------------------------------------------*
*                          Move Matnr p/ Objek                         *
*----------------------------------------------------------------------*
FORM z_move_matnr_objek.

  DATA: sl_mchb  TYPE type_mchb,
        vl_index TYPE i.

  LOOP AT t_mchb INTO sl_mchb.
    vl_index = sy-tabix.

    sl_mchb-objek = sl_mchb-matnr.

    MODIFY t_mchb FROM sl_mchb INDEX vl_index TRANSPORTING objek.

    CLEAR sl_mchb.
  ENDLOOP.

ENDFORM.                    " Z_MOVE_MATNR_OBJEK

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_CARAC                                        *
*&---------------------------------------------------------------------*
*                     Seleciona Dados Características                  *
*----------------------------------------------------------------------*
FORM z_seleciona_carac.
  DATA: tl_mchb TYPE TABLE OF type_mchb,
        tl_kssk TYPE TABLE OF type_kssk.

  REFRESH: t_inob, t_kssk, t_klah, t_ksml, t_cabnt.
  CLEAR st_rmclm.

  CHECK NOT t_mchb[] IS INITIAL.
  tl_mchb[] = t_mchb[].
  SORT tl_mchb BY objek ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_mchb COMPARING objek.

  SELECT SINGLE a~klart FROM tcla AS a
    INNER JOIN tclao AS b ON a~klart EQ b~klart
    INTO st_rmclm-klart
  WHERE a~obtab    EQ 'MCHA'
    AND a~intklart EQ space
    AND a~multobj  EQ 'X'
    AND b~obtab    EQ 'MCH1'.

  SELECT cuobj klart obtab objek
  FROM inob
    INTO TABLE t_inob
    FOR ALL ENTRIES IN tl_mchb
   WHERE klart EQ st_rmclm-klart
     AND obtab EQ 'MARA'
     AND objek EQ tl_mchb-objek.

  CHECK NOT t_inob[] IS INITIAL.
* Move Cuobj p/ Objekk
  PERFORM z_move_cuobj_objekk.

  SELECT objek mafid klart clint adzhl
  FROM kssk
    INTO TABLE t_kssk
    FOR ALL ENTRIES IN t_inob
  WHERE objek EQ t_inob-objekk
    AND mafid EQ 'O'
    AND klart EQ st_rmclm-klart.

  CHECK NOT t_kssk[] IS INITIAL.
  tl_kssk[] = t_kssk[].
  SORT tl_kssk BY clint ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_kssk COMPARING clint.

  SELECT clint klart class vondt bisdt
  FROM klah
    INTO TABLE t_klah
    FOR ALL ENTRIES IN tl_kssk
   WHERE clint EQ tl_kssk-clint.

  SELECT clint posnr adzhl imerk klart
  FROM ksml
    INTO TABLE t_ksml
    FOR ALL ENTRIES IN tl_kssk
  WHERE clint EQ tl_kssk-clint
    AND klart EQ st_rmclm-klart.

  CHECK NOT t_ksml[] IS INITIAL.

  SELECT a~atinn a~spras a~adzhl a~atbez
         b~atnam b~atfor
  FROM cabnt AS a
    INNER JOIN cabn AS b ON a~atinn EQ b~atinn
                        AND a~adzhl EQ b~adzhl
    INTO TABLE t_cabnt
    FOR ALL ENTRIES IN t_ksml
  WHERE a~atinn EQ t_ksml-imerk
    AND a~spras EQ 'PT'.

ENDFORM.                    " Z_SELECIONA_CARAC

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_CUOBJ_OBJEKK                                      *
*&---------------------------------------------------------------------*
*                          Move Cuobj p/ Objekk                        *
*----------------------------------------------------------------------*
FORM z_move_cuobj_objekk.
  DATA: sl_inob  TYPE type_inob,
        vl_index TYPE i.

  LOOP AT t_inob INTO sl_inob.
    vl_index = sy-tabix.

    sl_inob-objekk = sl_inob-cuobj.

    MODIFY t_inob FROM sl_inob INDEX vl_index TRANSPORTING objekk.

    CLEAR sl_inob.
  ENDLOOP.

ENDFORM.                    " Z_MOVE_CUOBJ_OBJEKK

*&---------------------------------------------------------------------*
*&      Form  Z_TRCA_P_V                                               *
*&---------------------------------------------------------------------*
*                               Troca , por .                          *
*----------------------------------------------------------------------*
FORM z_trca_p_v CHANGING p_campo TYPE char11.
  REPLACE ',' IN p_campo WITH '.'.
ENDFORM.                    " Z_TRCA_P_V

*&---------------------------------------------------------------------*
*&      Form  Z_RETORNA_MSG                                            *
*&---------------------------------------------------------------------*
*                               Retorna MSG                            *
*----------------------------------------------------------------------*
FORM z_retorna_msg TABLES p_ret   STRUCTURE bapiret2
                    USING p_matnr TYPE mchb-matnr
                          p_charg TYPE mchb-charg
                          p_werks TYPE mchb-werks.

  DATA: sl_ret TYPE bapiret2,
        sl_msg TYPE type_msg.

  LOOP AT p_ret INTO sl_ret.
    sl_msg-material = p_matnr.
    sl_msg-lote     = p_charg.
    sl_msg-centro   = p_werks.
    sl_msg-tipo     = sl_ret-type.
    sl_msg-mesagem  = sl_ret-message.

    APPEND sl_msg TO t_msg.

    CLEAR: sl_ret, sl_msg.
  ENDLOOP.

ENDFORM.                    " Z_RETORNA_MSG

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_LOTE                                          *
*&---------------------------------------------------------------------*
*                                Verifica Lote                         *
*----------------------------------------------------------------------*
FORM z_verifica_lote.

  DATA: tl_file TYPE TABLE OF zmmi0001,
        sl_0001 TYPE zmmi0001.

  tl_file[] = t_file[].
  SORT tl_file BY fardos ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_file COMPARING fardos.

  LOOP AT tl_file INTO sl_0001.

    READ TABLE t_mchb WITH KEY charg = sl_0001-fardos
                      BINARY SEARCH
                      TRANSPORTING NO FIELDS.

    CHECK NOT sy-subrc IS INITIAL.

*   Retorna MSG
    PERFORM z_retorna_msg_2 USING text-006
                                  sl_0001-fardos.

    CLEAR sl_0001.
  ENDLOOP.

ENDFORM.                    " Z_VERIFICA_LOTE

*&---------------------------------------------------------------------*
*&      Form  Z_RETORNA_MSG_2                                          *
*&---------------------------------------------------------------------*
*                               Retorna MSG                            *
*----------------------------------------------------------------------*
FORM z_retorna_msg_2 USING p_text  TYPE c
                           p_charg TYPE mchb-charg.

  DATA sl_msg TYPE type_msg.

  sl_msg-lote     = p_charg.
  sl_msg-centro   = s_werks-low.
  sl_msg-tipo     = 'E'.
  sl_msg-mesagem  = p_text.

  APPEND sl_msg TO t_msg.

ENDFORM.                    " Z_RETORNA_MSG_2

*&---------------------------------------------------------------------*
*&      Form  F_GERA_DIGITO
*&---------------------------------------------------------------------*
FORM f_gera_dig_fard CHANGING p_digito.

  FREE: p_digito.

*------------------------------------
* tabela de digitos
*------------------------------------
  t_char = VALUE #( ( pos = 00 chr = '0' ) ( pos = 01 chr = '1' ) ( pos = 02 chr = '2' )
                    ( pos = 03 chr = '3' ) ( pos = 04 chr = '4' ) ( pos = 05 chr = '5' )
                    ( pos = 06 chr = '6' ) ( pos = 07 chr = '7' ) ( pos = 08 chr = '8' )
                    ( pos = 09 chr = '9' ) ( pos = 10 chr = 'A' ) ( pos = 11 chr = 'B' )
                    ( pos = 12 chr = 'C' ) ( pos = 13 chr = 'D' ) ( pos = 14 chr = 'E' )
                    ( pos = 15 chr = 'F' ) ( pos = 16 chr = 'G' ) ( pos = 17 chr = 'H' )
                    ( pos = 18 chr = 'I' ) ( pos = 19 chr = 'J' ) ( pos = 20 chr = 'K' )
                    ( pos = 21 chr = 'L' ) ( pos = 22 chr = 'M' ) ( pos = 23 chr = 'N' )
                    ( pos = 24 chr = 'O' ) ( pos = 25 chr = 'P' ) ( pos = 26 chr = 'Q' )
                    ( pos = 27 chr = 'R' ) ( pos = 28 chr = 'S' ) ( pos = 29 chr = 'T' )
                    ( pos = 30 chr = 'U' ) ( pos = 31 chr = 'V' ) ( pos = 32 chr = 'W' )
                    ( pos = 33 chr = 'X' ) ( pos = 34 chr = 'Y' ) ( pos = 35 chr = 'Z' ) ).

*------------------------------------
* bloqueio numeracao
*------------------------------------
  DO.
    CALL FUNCTION 'ENQUEUE_EZPPT0034'
      EXPORTING
        mode_zppt0034  = 'E'
        mandt          = sy-mandt
        tipo           = '01'
        _scope         = '2'
        _wait          = 'X'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    EXIT.
  ENDDO.

*------------------------------------
* ultimo numero
*------------------------------------
  FREE: l_digito, w_zppt0034.

  SELECT SINGLE *
    FROM zppt0034
    INTO w_zppt0034
   WHERE tipo = '01'.

  IF sy-subrc <> 0.
    l_digito = '000'.
  ELSE.
    l_digito = w_zppt0034-digito.
  ENDIF.

*------------------------------------
* Desbloqueio numeracao
*------------------------------------
  CALL FUNCTION 'DEQUEUE_EZPPT0034'
    EXPORTING
      mode_zppt0034 = 'E'
      mandt         = sy-mandt
      tipo          = '01'
      _scope        = '3'.

*------------------------------------
* monta sequencia
*------------------------------------
  l_ok   = abap_false.
  l_dig1 = t_char[ chr = l_digito+0(1) ]-pos - 1.
  l_dig2 = t_char[ chr = l_digito+1(1) ]-pos - 1.
  l_dig3 = t_char[ chr = l_digito+2(1) ]-pos.

  DO.
    IF l_ok = abap_true.
      EXIT.
    ENDIF.

    l_dig1 = l_dig1 + 1.

    IF l_dig1 > 35.
      l_dig1 = 0.
      EXIT.
    ENDIF.

    DO.
      IF l_ok = abap_true.
        EXIT.
      ENDIF.

      l_dig2 = l_dig2 + 1.

      IF l_dig2 > 35.
        l_dig2 = -1.
        l_dig3 = 0.
        EXIT.
      ENDIF.

      DO.
        l_dig3 = l_dig3 + 1.

        IF l_dig3 > 35.
          l_dig3 = -1.
          EXIT.
        ENDIF.

        l_chr1 = t_char[ pos = l_dig1 ]-chr.
        l_chr2 = t_char[ pos = l_dig2 ]-chr.
        l_chr3 = t_char[ pos = l_dig3 ]-chr.
        l_ok   = abap_true.
        EXIT.
      ENDDO.
    ENDDO.
  ENDDO.

  p_digito = l_chr1 && l_chr2 && l_chr3.

  w_zppt0034-mandt   = sy-mandt.
  w_zppt0034-tipo    = '01'.
  w_zppt0034-digito  = p_digito.
  MODIFY zppt0034 FROM w_zppt0034.

  COMMIT WORK AND WAIT.

ENDFORM.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
