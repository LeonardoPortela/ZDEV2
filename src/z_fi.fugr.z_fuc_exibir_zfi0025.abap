FUNCTION z_fuc_exibir_zfi0025.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_NRO_SOL) TYPE  NUM10
*"----------------------------------------------------------------------

  TYPES: BEGIN OF y_msg,
           msg TYPE t100-text.
  TYPES END OF y_msg.

  DATA: it_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
        t_messtab  TYPE TABLE OF          bdcmsgcoll,
        wa_bdcdata LIKE LINE OF           it_bdcdata,
        it_msg     TYPE TABLE OF bdcmsgcoll,
        tl_msg     TYPE TABLE OF y_msg.

  DATA: p_resp, check, p_erro(1).

  check i_nro_sol is not INITIAL.

  i_nro_sol = |{ i_nro_sol ALPHA = IN }|.

  FREE: it_bdcdata.
  APPEND VALUE #( program = '        '   dynpro = '    '   dynbegin = 'T'     fnam = 'ZFI0025	         '  fval = '                                     ') TO it_bdcdata.
  APPEND VALUE #( program = 'ZFIR0031'   dynpro = '0100'   dynbegin = 'X'     fnam = '                 '  fval = '                                     ') TO it_bdcdata.
  APPEND VALUE #( program = '        '   dynpro = '    '   dynbegin = ' '     fnam = 'BDC_CURSOR       '  fval = 'WG_CADLAN-NRO_SOL_CP                 ') TO it_bdcdata.
  APPEND VALUE #( program = '        '   dynpro = '    '   dynbegin = ' '     fnam = 'BDC_OKCODE       '  fval = '=DISPLA                              ') TO it_bdcdata.
*  APPEND VALUE #( program = '        '   dynpro = '    '   dynbegin = ' '     fnam = 'BDC_SUBSCR       '  fval = 'ZFIR0031                0110SU_ITENS ') TO it_bdcdata.
  APPEND VALUE #( program = 'ZFIR0031'   dynpro = '0100'   dynbegin = 'X'     fnam = '                 '  fval = '                                     ') TO it_bdcdata.
  APPEND VALUE #( program = '        '   dynpro = '    '   dynbegin = ' '     fnam = 'BDC_CURSOR       '  fval = 'WG_CADLAN-NRO_SOL                    ') TO it_bdcdata.
  APPEND VALUE #( program = '        '   dynpro = '    '   dynbegin = ' '     fnam = 'BDC_OKCODE       '  fval = '=SEARCH                              ') TO it_bdcdata.
  APPEND VALUE #( program = '        '   dynpro = '    '   dynbegin = ' '     fnam = 'WG_CADLAN-NRO_SOL'  fval = i_nro_sol                              ) TO it_bdcdata.

  REFRESH it_msg.

  CALL TRANSACTION 'ZFI0025' USING it_bdcdata
  MODE 'E'
  MESSAGES INTO it_msg
  UPDATE 'S'.

  IF line_exists( it_msg[ msgtyp = 'A' ] ).
    p_erro = abap_true.
  ELSE.
    IF line_exists( it_msg[ msgtyp = 'E' ] ).
      p_erro = abap_true.
    ENDIF.
  ENDIF.


ENDFUNCTION.
