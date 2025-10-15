"Name: \PR:SAPLSZA0\FO:ADDR_COMM_GET_INTERNAL\SE:BEGIN\EI
ENHANCEMENT 0 Z_SET_NOREPLY.
* 08.01.2020 Envio de e-mail SAP ( noreply@amaggi.com.br )
* Busca e-mail cadastrado para o usuário JOBADM e utiliza como remetente:

*  IF ( IADR6[] IS NOT INITIAL ).
*
*    DATA(ADR6_COUNT) = LINES( IADR6[] ).
*
*    IF ( ADR6_COUNT > 1 ).
*
*      READ TABLE IADR6[]
*       ASSIGNING FIELD-SYMBOL(<ls_ADR6>)
*       INDEX iv_person_index.
*       "WITH KEY ADDRNUMBER = iv_address_index
*       "         PERSNUMBER = iv_person_index  BINARY SEARCH.
*
*      IF ( SY-SUBRC = 0 ).
*
*        SELECT SINGLE U21~BNAME, U21~PERSNUMBER, U21~ADDRNUMBER,
*                      A6~SMTP_ADDR
*          FROM USR21 AS U21
*          INNER JOIN ADR6 AS A6
*          ON A6~ADDRNUMBER = U21~ADDRNUMBER AND
*            A6~PERSNUMBER = U21~PERSNUMBER
*          INTO @DATA(W_DEFAULT_USR)
*          WHERE U21~BNAME = 'JOBADM'.
*
*       IF ( SY-SUBRC = 0 ).
*         <LS_ADR6>-SMTP_ADDR = W_DEFAULT_USR-SMTP_ADDR.
*       ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.


ENDENHANCEMENT.
