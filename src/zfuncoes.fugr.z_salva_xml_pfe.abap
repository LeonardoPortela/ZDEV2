FUNCTION Z_SALVA_XML_PFE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_XML) TYPE  STRING
*"     REFERENCE(P_PROTOCOLO) TYPE  ZPROTOCOLO
*"     REFERENCE(P_USUARIO) TYPE  SYUNAME
*"     REFERENCE(P_URL) TYPE  STRING
*"     REFERENCE(P_TIPO) TYPE  CHAR4
*"--------------------------------------------------------------------
*  TYPES: BEGIN OF TY_XML,
*          MANDT         TYPE ZLEST0049-MANDT,
*          PROTOCOLO     TYPE ZLEST0049-PROTOCOLO,
*          DATA_REGISTRO TYPE ZLEST0049-DATA_REGISTRO,
*          HORA_REGISTRO TYPE ZLEST0049-HORA_REGISTRO,
*          USUARIO       TYPE ZLEST0049-USUARIO,
*          XML           TYPE STRING,
*          IP_ADDRESS    TYPE ZLEST0049-IP_ADDRESS,
*          DEPARTMENT    TYPE ZLEST0049-DEPARTMENT,
*          URL           TYPE ZLEST0049-URL       ,
*          TIPO          TYPE ZLEST0049-TIPO,
*         END OF TY_XML.
*
*  DATA: WA_XML          TYPE TY_XML,
*        P_ADDRESS       TYPE CHAR69,
*        WA_USER_ADDR    TYPE USER_ADDR.
*
*    CALL FUNCTION 'ZGET_IP_ADDRESS'
*      IMPORTING
*        ZIP_ADDRESS = P_ADDRESS.
*
*  SELECT SINGLE * FROM USER_ADDR
*    INTO WA_USER_ADDR
*    WHERE BNAME EQ P_USUARIO.
*
*  WA_XML-PROTOCOLO       = P_PROTOCOLO.
*  WA_XML-DATA_REGISTRO   = sy-datum.
*  WA_XML-HORA_REGISTRO   = sy-uzeit.
*  WA_XML-USUARIO         = p_usuario.
*  WA_XML-XML             = P_XML.
*  WA_XML-MANDT           = SY-MANDT.
*
*  WA_XML-IP_ADDRESS      = P_ADDRESS.
*  WA_XML-DEPARTMENT      = WA_USER_ADDR-DEPARTMENT.
*  wa_xml-tipo            = p_tipo.
*
*  IF NOT ( WA_XML IS INITIAL ).
*
*    INSERT ZLEST0049 FROM WA_XML.
*
*    IF ( SY-SUBRC EQ 0 ).
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.
*
*  ENDIF.
ENDFUNCTION.
