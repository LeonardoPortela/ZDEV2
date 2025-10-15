FUNCTION zsd_fat_ztro.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_SALES) TYPE  VBAK-VBELN OPTIONAL
*"  EXPORTING
*"     VALUE(P_FAT) TYPE  VBAK-VBELN
*"     VALUE(T_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  DATA: vl_mode   TYPE c VALUE 'N'        ,
        tl_itab   TYPE TABLE OF bdcmsgcoll,
        sl_itab   TYPE bdcmsgcoll         ,
        sl_return TYPE bapiret2           ,
        vl_data   type c length 10.

  REFRESH: t_bdc   ,
           t_return.
  CLEAR p_fat.

* Insere BDC
  "" VERIFICA PERMISSÃO DO USUÁRIO COM RELAÇÃO A DATA RETROATIVA
  "" AJUSTE POR ERRO (06/11/2014) DE BACKUP DO BANCO DB2
  DATA: WA_SETLEAF  TYPE SETLEAF.
  CLEAR: WA_SETLEAF.
  SELECT SINGLE *
    FROM SETLEAF
    INTO WA_SETLEAF
   WHERE SETNAME = 'VF01_USUARIO'
     AND VALFROM = SY-UNAME.

  IF WA_SETLEAF IS INITIAL.
    concatenate sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) into vl_data.
  ELSE.
    PERFORM BUSCA_MEMORIA_DATA_RETRO_2 CHANGING vl_data.
  ENDIF.

  PERFORM z_insere_bdc USING: 'X' 'SAPMV60A'        '0102'  ,
                              ' '	 'BDC_CURSOR'	      'RV60A-FKDAT',
                              ' '  'RV60A-FKDAT'      vl_data,

                              ' '  'BDC_OKCODE'       '=SICH',
                              ' '  'KOMFK-VBELN(01)'  p_sales.

  CALL TRANSACTION 'VF01'
    USING t_bdc
    MODE vl_mode
    MESSAGES INTO tl_itab.

  READ TABLE tl_itab INTO sl_itab
    WITH KEY msgnr = '311'.

  IF sy-subrc IS INITIAL.
    p_fat = sl_itab-msgv1.
  ELSE.
    LOOP AT tl_itab INTO sl_itab.
      sl_return-type       = sl_itab-msgtyp.
      sl_return-id         = sl_itab-msgid.
      sl_return-number     = sl_itab-msgnr.
      sl_return-message_v1 = sl_itab-msgv1.
      sl_return-message_v2 = sl_itab-msgv2.
      sl_return-message_v3 = sl_itab-msgv3.
      sl_return-message_v4 = sl_itab-msgv4.
      MESSAGE ID sl_return-id
        TYPE sl_return-type
      NUMBER sl_return-number
        WITH sl_return-message_v1
             sl_return-message_v2
             sl_return-message_v3
             sl_return-message_v4
        INTO sl_return-message.
      APPEND sl_return TO t_return.
      CLEAR: sl_itab  ,
             sl_return.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
