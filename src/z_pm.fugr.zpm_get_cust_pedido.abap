FUNCTION zpm_get_cust_pedido.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_AUFNR) TYPE  AUFNR OPTIONAL
*"     REFERENCE(I_BUDAT_INI) TYPE  CO_BUDAT
*"     REFERENCE(I_BUDAT_FIM) TYPE  CO_BUDAT
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     VALUE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(I_EBELN) TYPE  EBELN OPTIONAL
*"  TABLES
*"      E_KAEP_COAC STRUCTURE  KAEP_COAC
*"      E_PEDIDO STRUCTURE  ZPME0073 OPTIONAL
*"      I_AUFK STRUCTURE  AUFK OPTIONAL
*"----------------------------------------------------------------------
  DATA: r_budat TYPE RANGE OF cobk-budat,
        r_bukrs TYPE RANGE OF bukrs,
        r_werks TYPE RANGE OF werks_d,
        r_ebeln TYPE RANGE OF ebeln,
        r_aufnr TYPE RANGE OF aufnr.

  DATA: i_area  TYPE tka01-kokrs VALUE 'MAGI',
        i_tcode TYPE sy-tcode    VALUE 'KOB1'.

  FREE: it_saida[], E_KAEP_COAC.


  r_budat = VALUE #( ( sign = 'I' option = 'BT' low = i_budat_ini high = i_budat_fim ) ).
  r_bukrs = VALUE #( ( sign = 'I' option = 'EQ' low = i_bukrs     ) ).
  if i_werks is not INITIAL.
  r_werks = VALUE #( ( sign = 'I' option = 'EQ' low = i_werks     ) ).
  endif.
  r_ebeln = VALUE #( FOR I IN E_PEDIDO ( sign = 'I' option = 'EQ' low = I-EBELN ) ).

  IF I_AUFK[] IS NOT INITIAL.
    r_aufnr = VALUE #( FOR L IN I_AUFK (  sign = 'I' option = 'EQ' low = L-AUFNR ) ).
  ENDIF.
  IF i_aufnr IS NOT INITIAL.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = i_aufnr ) TO r_aufnr.
  ENDIF.

  PERFORM f_prepare_run_time_info.

  SUBMIT rkaep000
             WITH p_tcode   EQ i_tcode
             WITH p_kokrs   EQ i_area
             WITH bukrs     IN r_bukrs
             WITH werks     IN r_werks
             WITH aufnr     IN r_aufnr
*             WITH i_ebeln   IN r_ebeln
             WITH r_budat   IN r_budat
             WITH p_maxsel  EQ 999999
             WITH p_usedb   EQ 'X'
             WITH p_disvar  EQ '/CONFERENCIA'
             EXPORTING LIST TO MEMORY AND RETURN.

  PERFORM f_get_runtime_info.

  IF <t_data> IS ASSIGNED.
    LOOP AT <t_data> ASSIGNING <w_data>.
      CLEAR: it_saida.
      MOVE-CORRESPONDING <w_data> TO it_saida.
      APPEND it_saida.
    ENDLOOP.
  ENDIF.

  SORT it_saida by vrgng.
  DELETE it_saida WHERE vrgng NE 'COIN'.

  SORT it_saida by ebeln.
  DELETE it_saida WHERE ebeln not in r_ebeln.

  SORT it_saida by WTGBTR.
  DELETE it_saida WHERE WTGBTR <= '0.01'.

  IF it_saida[] IS NOT INITIAL.
    APPEND LINES OF it_saida TO E_KAEP_COAC.
  ENDIF.
ENDFUNCTION.
