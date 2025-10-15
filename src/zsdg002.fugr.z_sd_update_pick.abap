FUNCTION z_sd_update_pick.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(VBELN) TYPE  LIPS-VBELN
*"     VALUE(PICK) TYPE  VBFA-RFMNG
*"     VALUE(NO_WAIT) TYPE  BAPIFLAG-BAPIFLAG OPTIONAL
*"     VALUE(V_LGORT) TYPE  LGORT_D OPTIONAL
*"     VALUE(V_ZSDT0001) TYPE  ZSDT0001 OPTIONAL
*"  TABLES
*"      RETURN STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  DATA: v_mode TYPE c VALUE 'N',
        "v_editar(1) TYPE c,
        v_pick(20) TYPE c.

* Tabelas Internas
  DATA: tl_lines  TYPE TABLE OF tline.
  DATA: itab TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

* Estruturas
  DATA: st_header TYPE thead,
        st_lines  TYPE tline.
* Variável
  DATA: vl_name(10)  TYPE c,
        vl_text(256) TYPE c.
** Constantes
  CONSTANTS: c_aster(1) TYPE c              VALUE '*'.

  REFRESH: t_bdc.

*  IF no_wait IS INITIAL.
*    WAIT UP TO 2 SECONDS.
*  ENDIF.

  WRITE pick TO v_pick.
  SHIFT v_pick LEFT DELETING LEADING space.

*  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'   '4004',
*                              ' ' 'BDC_CURSOR' 'LIKP-VBELN',
*                              ' ' 'BDC_OKCODE' '/00',
*                              ' ' 'LIKP-VBELN' vbeln.
*
*  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'        '1000',
*                              ' ' 'BDC_CURSOR'      'LIPSD-PIKMG(01)',
*                              ' ' 'BDC_OKCODE'      '=T\02'.
*
*  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'         '1000',
*                              ' ' 'BDC_CURSOR'       'LIPSD-PIKMG(01)',
*                              ' ' 'BDC_OKCODE'       '/00',
*                              ' ' 'LIPSD-G_LFIMG(01)' v_pick,
*                              ' ' 'LIPSD-PIKMG(01)'   v_pick.
*
*  IF NOT v_lgort IS INITIAL.
*    PERFORM z_insere_bdc USING: ' ' 'LIPS-LGORT(01)'    v_lgort,
*                                ' ' 'LIPS-CHARG(01)'    v_zsdt0001-nr_safra.
*  ENDIF.
*
*
*  PERFORM z_insere_bdc USING: ' ' 'BDC_OKCODE'  '=IDET_T',
*                              'X' 'SAPMV50A'    '3000',
*                              ' ' 'BDC_CURSOR'  'LIPS-NTGEW',
*                              ' ' 'LIPS-LFIMG'  v_pick,
*                              ' ' 'LIPS-BRGEW'  v_pick,
*                              ' ' 'LIPS-GEWEI'  'KG',
*                              ' ' 'LIPS-NTGEW'  v_pick,
*                              ' ' 'BDC_OKCODE'  '=BACK_T'.
*
*  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'        '1000',
*                              ' ' 'BDC_OKCODE'      '=HDET_T',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A'.
*
*  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'        '2000',
*
*                              ' ' 'BDC_OKCODE'      '=T\08',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A',
*                              'X' 'SAPMV50A'        '2000',
*                              ' ' 'BDC_OKCODE'      '=SICH_T',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A',
*                              ' ' 'BDC_SUBSCR'       'SAPLV09C',
*                              ' ' 'BDC_CURSOR'      'GVS_TC_DATA-REC-PARTNER(03)',
*                              ' ' 'GV_FILTER'        'PARALL',
*                              ' ' 'GVS_TC_DATA-REC-PARVW(02)'    'PC',
*                              ' ' 'GVS_TC_DATA-REC-PARTNER(02)'  v_zsdt0001-parid,
*                              ' ' 'GVS_TC_DATA-REC-PARVW(03)'    'LR',
*                              ' ' 'GVS_TC_DATA-REC-PARTNER(03)'  v_zsdt0001-id_cli_dest.
*
*
*  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'        '1000',
*                              ' ' 'BDC_OKCODE'      '=WABU_T',
*                              ' ' 'BDC_CURSOR'      'LIPSD-PIKMG(01)',
*                              ' ' 'BDC_SUBSCR'      'SAPMV50A'.

  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'   '4004',
                              ' ' 'BDC_CURSOR' 'LIKP-VBELN',
                              ' ' 'BDC_OKCODE' '/00',
                              ' ' 'LIKP-VBELN' vbeln.
  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'  '1000',
                              ' ' 'BDC_OKCODE'  '=IDET_T',
                              ' ' 'BDC_SUBSCR'  'SAPMV50A',
                              ' ' 'BDC_SUBSCR'  'SAPMV50A',
                              ' ' 'BDC_CURSOR'  'LIPS-POSNR(01)',
                              ' ' 'LIPS-LGORT(01)'  v_lgort,
                              ' ' 'LIPSD-PIKMG(01)'	v_pick,
                              ' ' 'BDC_SUBSCR'  'SAPMV50A'.
  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'  '3000',

                              ' ' 'BDC_OKCODE'  '=HDET_T',
                              ' ' 'BDC_SUBSCR'  'SAPMV50A',
                              ' ' 'BDC_CURSOR'  'LIPS-NTGEW',
                              ' ' 'LIPS-LFIMG'  v_pick,
                              ' ' 'LIPS-BRGEW'  v_pick,
                              ' ' 'LIPS-GEWEI'  'KG',
                              ' ' 'LIPS-NTGEW'  v_pick,
                              ' ' 'BDC_SUBSCR'  'SAPMV50A'.

  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'  '2000',
                              ' ' 'BDC_OKCODE'  '=T\08',
                              ' ' 'BDC_SUBSCR'  'SAPMV50A',
                              ' ' 'BDC_SUBSCR'  'SAPMV50A'.

  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'  '2000',
                              ' ' 'BDC_OKCODE'  '=T\09',
                              ' ' 'BDC_SUBSCR'  'SAPMV50A',
                              ' ' 'BDC_SUBSCR'  'SAPMV50A'.

  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'  '2000',
                              ' ' 'BDC_OKCODE'  '/00',
                              ' ' 'BDC_SUBSCR'  'SAPMV50A',
                              ' ' 'BDC_SUBSCR'  'SAPLV09C',
                              ' ' 'BDC_CURSOR'  'GVS_TC_DATA-REC-PARTNER(03)',
                              ' ' 'GV_FILTER'	'PARALL',
                              ' ' 'GVS_TC_DATA-REC-PARVW(02)'	'PC',
                              ' ' 'GVS_TC_DATA-REC-PARVW(03)'	'LR',
                              ' ' 'GVS_TC_DATA-REC-PARTNER(02)'	v_zsdt0001-parid,
                              ' ' 'GVS_TC_DATA-REC-PARTNER(03)'	v_zsdt0001-id_cli_dest.
  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'  '2000',
                              ' ' 'BDC_OKCODE'  '=WABU_T',
                              ' ' 'BDC_SUBSCR'  'SAPMV50A',
                              ' ' 'BDC_SUBSCR'  'SAPLV09C',
                              "' ' 'BDC_CURSOR'  'GVS_TC_DATA-REC-PARTNER(03)',
                              ' ' 'GV_FILTER'	'PARALL'.


  "v_editar  = 'X'.
  "EXPORT v_editar FROM v_editar TO MEMORY ID 'ZEDITAR'.

  CALL TRANSACTION 'VL02N' USING t_bdc MODE v_mode MESSAGES INTO itab.

  "DELETE FROM MEMORY ID 'ZEDITAR'.

  PERFORM set_msg_to_bapiret2 TABLES return itab.


* Verificar se existe pesagem do OPUS cadastrado
  IF NOT v_zsdt0001-ch_referencia IS INITIAL.

    REFRESH: tl_lines.
    CLEAR  : st_header, st_lines,
             vl_name,   vl_text.

    vl_name = vbeln.

    st_header-tdobject = 'VBBK'.
    st_header-tdname   = vl_name.
    st_header-tdid     = '0001'.
    st_header-tdspras  = 'P'.

    CONCATENATE 'Pesagem OPUS ChRef:' v_zsdt0001-ch_referencia
                INTO vl_text SEPARATED BY space.
    st_lines-tdformat  = c_aster.
    st_lines-tdline    = vl_text.
    APPEND st_lines TO tl_lines.
    CLEAR: st_lines,
           vl_text.

    CONCATENATE 'Número do Romaneio:' v_zsdt0001-nr_romaneio
                INTO vl_text SEPARATED BY space.
    st_lines-tdformat  = c_aster.
    st_lines-tdline    = vl_text.
    APPEND st_lines TO tl_lines.
    CLEAR: st_lines,
           vl_text.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = st_header
        savemode_direct = 'X'
      TABLES
        lines           = tl_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.

    IF sy-subrc IS INITIAL.
      SORT tl_lines BY tdformat ASCENDING.
    ENDIF.

* Modificar o campo STATUS e atualizar a tabela ZSDT0001
    "v_zsdt0001-status  = 'X'.
    "v_zsdt0001-doc_rem = vbeln.
    "MODIFY zsdt0001 FROM v_zsdt0001.
  ENDIF.


ENDFUNCTION.
