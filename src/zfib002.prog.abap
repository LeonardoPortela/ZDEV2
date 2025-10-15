************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...: Braxis It Services                                  *
* Responsável ...: Fabio Costa Vasconcellos - Consultor BW/ABAP        *
* Data desenv ...: 05.04.2007                                          *
* Tipo de prg ...: Carga de Dados via Batch Input
* Objetivo    ...: Coletar dados de Saldos Contábeis em planilha excel *
*                  e carregar esses saldos no R/3.                     *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 05.04.2007    Fabio C Vasconcellos First Code                        *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
REPORT  zfib002 LINE-SIZE 150
                LINE-COUNT 62(03)
                MESSAGE-ID z01 NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
INCLUDE <icon>.

DATA     : v_ncoln       LIKE sy-index VALUE 60.

CONSTANTS: c_ncoln       LIKE sy-index VALUE 01,
           c_nline       LIKE sy-index VALUE 01,
           c_ktopl       LIKE cska-ktopl VALUE '0050'. "PL.CONTAS

DATA: v_message(220)     TYPE c,
      v_mess_tab(256)    TYPE c,
      v_ctrlcol1         TYPE alsmex_tabline-value,
      v_icon             TYPE c.

FIELD-SYMBOLS <icone>    LIKE icon_checked.

TYPES: BEGIN OF ty_cska,
        ktopl LIKE cska-ktopl,
        kstar LIKE cska-kstar,
       END OF ty_cska.

DATA: ti_cska TYPE TABLE OF ty_cska WITH HEADER LINE.
*----------------------------------------------------------------------*
* Declaração para Batch_input de determinação de CFOP
*----------------------------------------------------------------------*
DATA: t_bdcdata          LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
      t_messtab          LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
      t_bdcaux           LIKE bdcdata    OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Declaração para função ALSM_EXCEL_TO_INTERNAL_TABLE
*----------------------------------------------------------------------*
DATA: t_planilha         LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.

DATA: t_datxls TYPE TABLE OF zecrgsldctb WITH HEADER LINE.
*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
PARAMETERS: p_cami  LIKE rlgrap-filename OBLIGATORY,  "Arquivo Excel
            p_nline LIKE sy-index        OBLIGATORY,  "Nro aprox Linhas
            p_mod1  LIKE ctu_params-dismode.
SELECTION-SCREEN END   OF BLOCK b0.

*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_cami.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_cami
      mask             = ',*.xls.'
      mode             = 'O'
      title            = 'Arquivo a importar !'
    IMPORTING
      filename         = p_cami
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
*
START-OF-SELECTION.

  PERFORM f_processa_planilha.
  SORT t_datxls BY line.

  IF NOT t_datxls[] IS INITIAL.
    PERFORM f_seleciona_pl_contas.

    PERFORM f_monta_carga_saldos.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  f_processa_planilha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_planilha.
*
* Carrega planilha com dados de clientes
*
  DATA: vl_bukrs LIKE bkpf-bukrs,
        vl_subrc LIKE sy-subrc.

  CLEAR   t_planilha.
  REFRESH t_planilha.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_cami
      i_begin_col             = c_ncoln
      i_begin_row             = c_nline
      i_end_col               = v_ncoln
      i_end_row               = p_nline
    TABLES
      intern                  = t_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE e004 WITH text-002.
  ENDIF.

  SORT t_planilha BY row col.
  REFRESH t_datxls.
  CLEAR   t_datxls.

*> Move valores das células
  LOOP AT t_planilha.

    ON CHANGE OF t_planilha-row.
      v_ctrlcol1 = t_planilha-value.
    ENDON.
*> Elimina linhas de cabeçalho e texto da tabela interna.
    IF v_ctrlcol1 EQ 'CAB'.
      DELETE t_planilha.
      CONTINUE.
    ENDIF.

    AT NEW row.
      CLEAR t_datxls.
      t_datxls-line = t_planilha-row.
    ENDAT.

    CASE t_planilha-col.
      WHEN  2.
        t_datxls-bukrs = t_planilha-value.
        IF vl_bukrs IS INITIAL.
          vl_bukrs = t_datxls-bukrs.
        ELSE.
          IF vl_bukrs NE t_datxls-bukrs.
* SE EXISTIREM DADOS DE MAIS DE UMA EMPRESA NA PLANILHA, RETORNAR
* MENSAGEM DE ERRO.
            MESSAGE e000.
          ENDIF.
        ENDIF.
      WHEN  3. t_datxls-bldat = t_planilha-value.
      WHEN  4. t_datxls-budat = t_planilha-value.
      WHEN  5. t_datxls-blart = t_planilha-value.
      WHEN  6. t_datxls-xblnr = t_planilha-value.
      WHEN  7. t_datxls-bktxt = t_planilha-value.
      WHEN  8. t_datxls-waers = t_planilha-value.
      WHEN  9. t_datxls-sgtxt = t_planilha-value.
      WHEN 10. t_datxls-bschl = t_planilha-value.
      WHEN 11. t_datxls-hkont = t_planilha-value.
      WHEN 12. t_datxls-wrbtr = t_planilha-value.
      WHEN 13. t_datxls-shkzg = t_planilha-value.
      WHEN 14. t_datxls-kostl = t_planilha-value.
      WHEN 15. t_datxls-aufnr = t_planilha-value.
      WHEN 16. t_datxls-prctr = t_planilha-value.
      WHEN 17. t_datxls-sgtxt = t_planilha-value.
      WHEN OTHERS.
    ENDCASE.

    AT END OF row.
      APPEND t_datxls.
    ENDAT.

  ENDLOOP.
ENDFORM.                    "f_processa_planilha


*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
FORM f_bdc_field USING    value(p_flag)
                          value(p_fnam)
                          value(p_fval).

  CLEAR t_bdcdata.
  IF NOT p_flag IS INITIAL.
    t_bdcdata-program  = p_fnam.
    t_bdcdata-dynpro   = p_fval.
    t_bdcdata-dynbegin = 'X'.
  ELSE.
    t_bdcdata-fnam = p_fnam.
    t_bdcdata-fval = p_fval.
  ENDIF.
  APPEND t_bdcdata.

ENDFORM.                    "f_bdc_field

*&---------------------------------------------------------------------*
*&      Form  f_monta_carga_saldos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_carga_saldos .

  DATA: vl_saldo   LIKE bseg-dmbtr,
        vl_counter LIKE sy-tabix,
        vl_subrc   LIKE sy-subrc,
        vl_newbs   LIKE rf05a-newbs,
        vl_flagfst TYPE c,
        vl_prctr   LIKE cobl-prctr,
        vl_kostl   LIKE cobl-kostl,
        vl_aufnr   LIKE cobl-aufnr,

        vl_tabix   LIKE sy-tabix,
        vl_hkont   LIKE bseg-hkont,
        vl_bschl   LIKE bseg-bschl,
        vl_wrbtr(17) TYPE c.

  DATA: lt_datxls TYPE TABLE OF zecrgsldctb WITH HEADER LINE.

  lt_datxls[] = t_datxls[].

  LOOP AT t_datxls.
* A primeira tela será chamada somente para a primeira linha da
* tabela e quando for necessário iniciar um documento novo (ocorrerá
* nos casos de mais de 998 itens)
    vl_tabix = sy-tabix + 1.

    IF vl_flagfst IS INITIAL.
      vl_flagfst = 'X'.

      PERFORM f_bdc_field USING: 'X' 'SAPMF05A'       '0100',
                                 ' ' 'BKPF-BLDAT'     t_datxls-bldat,
                                 ' ' 'BKPF-BLART'     t_datxls-blart,
                                 ' ' 'BKPF-BUKRS'     t_datxls-bukrs,
                                 ' ' 'BKPF-BUDAT'     t_datxls-budat,
                                 ' ' 'BKPF-WAERS'     t_datxls-waers,
                                 ' ' 'BKPF-XBLNR'     t_datxls-xblnr,
                                 ' ' 'BKPF-BKTXT'     t_datxls-bktxt,
                                 ' ' 'RF05A-NEWBS'    t_datxls-bschl,
                                 ' ' 'RF05A-NEWKO'    t_datxls-hkont,
                                 ' ' 'BDC_OKCODE'     '/00'.

      ADD 1 TO vl_counter.
    ENDIF.
* PREENCHIMENTO DO CENTRO DE LUCRO
    IF t_datxls-hkont+0(1) = '3'.
      vl_prctr = t_datxls-prctr.
    ELSE.
      CLEAR vl_prctr.
    ENDIF.

* PREENCHIMENTO DO CENTRO DE CUSTO/ORDEM INTERNA, SOMENTE SE A CONTA
* INFORMADA FOR ENCONTRADA NO PLANO DE CONTAS 0050
    READ TABLE ti_cska WITH KEY kstar = t_datxls-hkont BINARY SEARCH.
    IF sy-subrc EQ 0.
      vl_kostl = t_datxls-kostl.
      vl_aufnr = t_datxls-aufnr.
    ELSE.
      CLEAR: vl_kostl,
             vl_aufnr.
    ENDIF.

    vl_wrbtr = t_datxls-wrbtr.
    TRANSLATE vl_wrbtr USING '.,'.
* LÊ O REGISTRO SEGUINTE, PARA BUSCAR A CHAVE A CONTA DE LANÇAMENTO
    READ TABLE lt_datxls INDEX vl_tabix.
    IF sy-subrc NE 0.
      CLEAR: vl_bschl,
             vl_hkont.
    ELSE.
      vl_bschl = lt_datxls-bschl.
      vl_hkont = lt_datxls-hkont.
    ENDIF.

    PERFORM f_bdc_field USING: 'X' 'SAPMF05A'       '0300',
                               ' ' 'BSEG-WRBTR'     vl_wrbtr,
                               ' ' 'BSEG-SGTXT'     t_datxls-sgtxt,
                               ' ' 'RF05A-NEWBS'    vl_bschl,
                               ' ' 'RF05A-NEWKO'    vl_hkont,
                               ' ' 'BDC_OKCODE'     '/00',

                               'X' 'SAPLKACB'      '0002',
                               ' ' 'COBL-PRCTR'     vl_prctr,
                               ' ' 'COBL-KOSTL'     vl_kostl,
                               ' ' 'COBL-AUFNR'     vl_aufnr,
                               ' ' 'BDC_OKCODE'    '/00'.

    ADD 1 TO vl_counter.


** A PRINCIPIO, A ROTINA ABAIXO NÃO SERÁ NECESSÁRIA. SERÁ SOLICITADO AO
** ADM DO LEGADO QUE ENVIE A CARGA EM LOTES DE 990 ITENS.

**    IF t_datxls-shkzg EQ 'S'. "Débito
**      vl_saldo = vl_saldo + t_datxls-wrbtr.
**    ELSE.
**      vl_saldo = vl_saldo - t_datxls-wrbtr.
**    ENDIF.
*** QUANDO ATINGIR 998 ITENS, FECHAR O DOCUMENTO COM UMA CONTRAPARTIDA
*** SE A SOMA DOS ITENS ATÉ ENTÃO FOR POSITIVA, FAZER UM LANÇAMENTO DE
*** CRÉDITO COMO CONTRAPARTIDA (CHAVE 50)
*** SENÃO, FAZER UM LANÇAMENTO DE DÉBITO (CHAVE 40)
**    IF vl_counter GE 998.
**      IF vl_saldo GT 0.
**        vl_newbs = '50'.
**      ELSE.
**        vl_newbs = '40'.
**      ENDIF.
**
**      PERFORM f_bdc_field USING: 'X' 'SAPMF05A'       '0300',
**                                 ' ' 'BSEG-WRBTR'     vl_saldo,
**                                 ' ' 'BSEG-SGTXT'     'Contra-partida'
*,
**                                 ' ' 'RF05A-NEWBS'    vl_newbs,
**                                 ' ' 'RF05A-NEWKO'    t_datxls-hkont,
**                                 ' ' 'BDC_OKCODE'     '/00'.
**
**      PERFORM f_bdc_field USING: 'X' 'SAPMF05A'       '0300',
**                                 ' ' 'BDC_OKCODE'     '=BU'.
**
**      CLEAR: vl_flagfst,
**             vl_saldo,
**             vl_counter.
**    ENDIF.
  ENDLOOP.

  IF sy-subrc EQ 0.
    PERFORM f_bdc_field USING: 'X' 'SAPMF05A'       '0300',
                               ' ' 'BDC_OKCODE'     '=BU'.
  ENDIF.

  CALL TRANSACTION 'F-02' USING t_bdcdata
                           MODE p_mod1
                         UPDATE 'S'
                  MESSAGES INTO t_messtab.

  LOOP AT t_messtab.
    IF 'WAE' CS t_messtab-msgtyp.
      ASSIGN icon_incomplete TO <icone>.
    ELSEIF t_messtab-msgtyp EQ 'S'.
      ASSIGN icon_checked TO <icone>.
    ELSEIF t_messtab-msgtyp EQ 'I'.
      ASSIGN icon_failure TO <icone>.
    ENDIF.

    sy-msgid = t_messtab-msgid.
    sy-msgno = t_messtab-msgnr.
    sy-msgv1 = t_messtab-msgv1.
    sy-msgv2 = t_messtab-msgv2.
    sy-msgv3 = t_messtab-msgv3.
    sy-msgv4 = t_messtab-msgv4.
    CALL FUNCTION 'CUTC_GET_MESSAGE'
      EXPORTING
        msg_id      = sy-msgid
        msg_no      = sy-msgno
        msg_arg1    = sy-msgv1
        msg_arg2    = sy-msgv2
        msg_arg3    = sy-msgv3
        msg_arg4    = sy-msgv4
        language    = sy-langu
      IMPORTING
        raw_message = v_mess_tab.
    CONCATENATE text-009 v_mess_tab INTO v_mess_tab
                                    SEPARATED BY space.
    PERFORM f_imprime_erros  USING v_mess_tab.
  ENDLOOP.


ENDFORM.                    " f_monta_carga_saldos

*&---------------------------------------------------------------------*
*&      Form  f_seleciona_pl_contas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_pl_contas .
  SELECT ktopl kstar
    FROM cska
    INTO TABLE ti_cska
     FOR ALL ENTRIES IN t_datxls
   WHERE ktopl EQ c_ktopl                                   "0050
     AND kstar EQ t_datxls-hkont.

  SORT ti_cska BY kstar ASCENDING.
ENDFORM.                    " f_seleciona_pl_contas

*&---------------------------------------------------------------------*
*&      Form  f_imprime_erros
*&---------------------------------------------------------------------*
FORM f_imprime_erros USING    p_message.
  WRITE: /01 sy-vline,
          03 <icone> AS ICON,
          08 t_datxls-line,
          23 t_datxls-bukrs,
          28 t_datxls-hkont,
          63 p_message,
         150 sy-vline.
ENDFORM.                    " f_imprime_erros
