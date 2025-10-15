function zpp_change_mr22_usd.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_MATNR) TYPE  MATNR
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"     VALUE(I_DATA) TYPE  BUDAT
*"     REFERENCE(I_AMOUNT) TYPE  CKMPC_ZUUMB
*"     REFERENCE(I_TEXT) TYPE  BKTXT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_BELNR) TYPE  CK_BELNR
*"----------------------------------------------------------------------
  data: zva_date  type char10,
        zva_value type char10.

  if i_matnr  is not initial
  and i_werks is not initial
  and i_data  is not initial
  and i_amount is not initial.

    free: it_bdcdata.
    clear: zva_date, zva_value.
    zva_date = |{ i_data+6(2) }.{ i_data+4(2) }.{ i_data+0(4) }|.
    zva_value = i_amount.
    replace all occurrences of '.' in zva_value with ' ,' .
    condense zva_value no-gaps.
    if i_amount < 0.
      zva_value = |{ zva_value }-|.
    endif.

    perform zf_bdc_data using:
    '            '  '    '  'T '    'MR22	                  '  '                                             ',
    'SAPRCKM_MR22'  '0201'  'X '    '                       '  '                                             ',
    '            '  '    '  '  '    'BDC_CURSOR	            '  'MR21HEAD-BUDAT                               ',
    '            '  '    '  '  '    'BDC_OKCODE             '  '=ENTR                                        ',
    '            '  '    '  '  '    'MR21HEAD-BUDAT	        '  zva_date                                       ,
    '            '  '    '  '  '    'MR21HEAD-WERKS	        '  i_werks                                        ,
    '            '  '    '  '  '    'MR21HEAD-BKTXT         '  i_text                                         , "US #156567 - MMSILVA - 02.05.2025
    '            '  '    '  '  '    'MR21HEAD-XBLNR         '  i_text                                         , "US #156567 - MMSILVA - 02.05.2025
    '            '  '    '  '  '    'BDC_SUBSCR	            '  'SAPRCKM_MR22                    0250MR22_SUB ',
    '            '  '    '  '  '    'MR21HEAD-SCREEN_VARIANT'  'LAGERMATERIAL_0250                           ',
    'SAPRCKM_MR22'  '0201'  'X '    '                       '  '                                             ',
    '            '  '    '  '  '    'BDC_OKCODE	            '  '=TAB2                                        ',
    '            '  '    '  '  '    'BDC_SUBSCR	            '  'SAPRCKM_MR22                    0250MR22_SUB ',
    '            '  '    '  '  '    'BDC_CURSOR	            '  'CKI_MR22_0250-MATNR(01)                      ',
    '            '  '    '  '  '    'MR21HEAD-SCREEN_VARIANT'  'LAGERMATERIAL - OHNE BWKEY_025               ',
    'SAPRCKM_MR22'  '0201'  'X '    '                       '  '                                             ',
    '            '  '    '  '  '    'BDC_OKCODE	            '  '=ENTR                                        ',
    '            '  '    '  '  '    'BDC_SUBSCR	            '  'SAPRCKM_MR22                    0250MR22_SUB ',
    '            '  '    '  '  '    'BDC_CURSOR	            '  'CKI_MR22_0250-ZUUMB(01)                      ',
    '            '  '    '  '  '    'MR21HEAD-SCREEN_VARIANT'  'LAGERMATERIAL - OHNE BWKEY_025               ',
    '            '  '    '  '  '    'CKI_MR22_0250-MATNR(01)'  i_matnr                                        ,
    '            '  '    '  '  '    'CKI_MR22_0250-ZUUMB(01)'  zva_value                                      ,
    'SAPRCKM_MR22'  '0201'  'X '    '                       '  '                                             ',
    '            '  '    '  '  '    'BDC_OKCODE	            '  '=SAVE                                        ',
    '            '  '    '  '  '    'BDC_SUBSCR             '  'SAPRCKM_MR22                    0250MR22_SUB ',
    '            '  '    '  '  '    'BDC_CURSOR             '  'CKI_MR22_0250-MATNR(02)                      ',
    '            '  '    '  '  '    'MR21HEAD-SCREEN_VARIANT'  'LAGERMATERIAL - OHNE BWKEY_025               ',
    'SAPLSPO1    '  '0100'  'X '    '                       '  '                                             ',

    '            '  '    '  '  '    'BDC_OKCODE	            '  '=YES                                         '.

    data lv_mode type char1.
    lv_mode = 'N'.

    perform zf_call_transaction using 'MR22' lv_mode 'S'.

    if it_msg[] is not initial.
      read table it_msg into data(wa_msg) with key msgtyp = 'S'.
      if sy-subrc eq 0.
        e_belnr = wa_msg-msgv1.
      endif.
    endif.
  endif.
endfunction.
