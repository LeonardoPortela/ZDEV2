FUNCTION Z_SD_UPDATE_PICK_2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(VBELN) TYPE  LIPS-VBELN
*"     VALUE(PICK) TYPE  VBFA-RFMNG
*"----------------------------------------------------------------------
  DATA: v_mode TYPE c VALUE 'N',
        v_pick(20) TYPE c.

  REFRESH: t_bdc.

  WRITE pick TO v_pick.
  SHIFT v_pick LEFT DELETING LEADING space.

  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'   '4004',
                              ' ' 'BDC_CURSOR' 'LIKP-VBELN',
                              ' ' 'BDC_OKCODE' '/00',
                              ' ' 'LIKP-VBELN' vbeln.

  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'        '1000',
                              ' ' 'BDC_CURSOR'      'LIPSD-PIKMG(01)',
                              ' ' 'BDC_OKCODE'      '/00',
                              ' ' 'LIPSD-PIKMG(01)' v_pick.

  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'        '1000',
                              ' ' 'BDC_CURSOR'      'LIPSD-PIKMG(01)',
                              ' ' 'BDC_OKCODE'      '=SICH_T'.


  CALL TRANSACTION 'VL02N' USING t_bdc MODE v_mode.



ENDFUNCTION.
