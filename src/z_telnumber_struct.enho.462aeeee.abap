"Name: \FU:TELNUMBER_STRUCT_TO_NORMAL\SE:END\EI
ENHANCEMENT 0 Z_TELNUMBER_STRUCT.
*

*** RJF - Ini - Correções baixa de usuário e criação de usuário SAP - Auditoria #136523
**DATA: lv_cpf_nr  TYPE pbr_cpfnr,
**      lv_cpf_nrf TYPE pbr_cpfnr,
**      lv_pernr   TYPE persno.
**
**FIELD-SYMBOLS: <fs_ustyp>     TYPE any,
**               <fs_bname>     TYPE any,
**               <fs_fax_numer> TYPE any,
**               <fs_floor>     TYPE any.
**
**ASSIGN ('(SAPLSUID_MAINTENANCE)SUID_ST_BNAME-BNAME')               TO <fs_bname>.
**ASSIGN ('(SAPLSUID_MAINTENANCE)SUID_ST_NODE_COMM_DATA-FAX_NUMBER') TO <fs_fax_numer>.
**ASSIGN ('(SAPLSUID_MAINTENANCE)SUID_ST_NODE_LOGONDATA-USTYP')      TO <fs_ustyp>.
**ASSIGN ('(SAPLSUID_MAINTENANCE)SUID_ST_NODE_WORKPLACE-FLOOR')      TO <fs_floor>.
**
**IF  <fs_ustyp> IS ASSIGNED AND <fs_ustyp> IS NOT INITIAL.
**
**  IF <fs_ustyp> EQ 'A'.
**    DATA(lv_valid) = abap_true.
**  ENDIF.
**
**ELSE.
**
**  IF  <fs_bname> IS ASSIGNED AND <fs_bname> IS NOT INITIAL.
**    SELECT ustyp FROM usr02
**      UP TO 1 ROWS
**      INTO @DATA(lv_ustyp)
**      WHERE bname EQ @<fs_bname>.
**    ENDSELECT.
**    IF sy-subrc IS INITIAL AND lv_ustyp EQ 'A'.
**      lv_valid = abap_true.
**    ELSE.
**      CLEAR lv_valid.
**    ENDIF.
**  ENDIF.
**ENDIF.
**
**IF  <fs_floor> IS ASSIGNED AND <fs_floor> IS NOT INITIAL.
**  IF ( ( <fs_floor> EQ 'AMAGGI_INT' ) OR
**     (   <fs_floor> EQ 'CONSULTOR' ) OR
**     (   <fs_floor> EQ 'MAPRENDIZ' ) OR
**     (   <fs_floor> EQ 'CONSELHO' ) OR
**     (   <fs_floor> EQ 'OUTROS' ) ).
**    lv_valid = abap_true.
**  ELSE.
**    CLEAR lv_valid.
**  ENDIF.
**ENDIF.
**
**IF lv_valid IS NOT INITIAL.
**  IF telnumber IS NOT INITIAL OR <fs_fax_numer> IS NOT INITIAL.
**    WRITE telnumber USING EDIT MASK
**          '___.___.___-__'
**          TO lv_cpf_nr.
**
***Ir na tabela PA0465 com CPF_NR = FAX
***Pegar PERNR
***Se encontrar, então pode salvar o registro.
**    SELECT pernr cpf_nr
**      UP TO 1 ROWS
**      FROM pa0465
**      INTO ( lv_pernr, lv_cpf_nrf )
**      WHERE cpf_nr EQ lv_cpf_nr.
**    ENDSELECT.
**    IF sy-subrc IS NOT INITIAL AND lv_cpf_nrf IS INITIAL.
***Se não encontrar, mensagem : "Não foi encontrado empregado com o CPF informado"
**
**      PERFORM error5
**              IN PROGRAM saplsza0
**              TABLES messages
**              USING 'ZCADASTRO'
**                    '001'
**                    lv_number_canonical_long
**                    space space space
**                    space
**                    'TELNUMBER'
**                    0.
**      PERFORM set_new_returncode
**              IN PROGRAM saplsza0
**              USING    c_error
**              CHANGING worst_error.
**
**
**    ENDIF.
**
**  ELSEIF sy-subrc IS INITIAL AND lv_cpf_nrf IS NOT INITIAL.
**
**    PERFORM error5
**            IN PROGRAM saplsza0
**            TABLES messages
**            USING 'ZCADASTRO'
**                  '003'
**                  lv_number_canonical_long
**                  space space space
**                  space
**                  'TELNUMBER'
**                  0.
**    PERFORM set_new_returncode
**            IN PROGRAM saplsza0
**            USING    c_error
**            CHANGING worst_error.
**
**  ELSEIF telnumber IS INITIAL. " e tipo user...
**
**    PERFORM error5
**            IN PROGRAM saplsza0
**            TABLES messages
**            USING 'ZCADASTRO'
**                  '002'
**                  lv_number_canonical_long
**                  space space space
**                  space
**                  'TELNUMBER'
**                  0.
**    PERFORM set_new_returncode
**            IN PROGRAM saplsza0
**            USING    c_error
**            CHANGING worst_error.
**
**  ENDIF.
**ENDIF.
*** RJF - Fim - Correções baixa de usuário e criação de usuário SAP - Auditoria #136523

ENDENHANCEMENT.
