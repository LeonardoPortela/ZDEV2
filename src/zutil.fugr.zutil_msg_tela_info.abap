FUNCTION ZUTIL_MSG_TELA_INFO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MENSAGEM_CAUSA) TYPE  STRING OPTIONAL
*"     REFERENCE(I_SYSTEM_RESPONSE) TYPE  STRING
*"     REFERENCE(I_WHAT_TO_DO) TYPE  STRING OPTIONAL
*"     REFERENCE(I_SYS_ADMIN) TYPE  STRING OPTIONAL
*"     REFERENCE(I_MENSAGEM_COMPLETA) TYPE  STRING OPTIONAL
*"----------------------------------------------------------------------
  DATA(CSS_PAGINA) = '<style>' &&
                     '   * {' &&
                     '     box-sizing: border-box;' &&
                     '   }' &&
                     '   ul {' &&
                     '     list-style-type: none;' &&
                     '     padding: 0;' &&
                     '     margin: 0;' &&
                     '   }' &&
                     '   ul li {' &&
                     '     border: 1px solid #ddd;' &&
                     '     margin-top: -1px; ' &&
                     '     background-color: #f6f6f6; ' &&
                     '     padding: 12px; ' &&
                     '   } ' &&
                     '   </style>'.

  DATA(HTML_BORY) = COND STRING( WHEN I_MENSAGEM_CAUSA IS INITIAL THEN SPACE ELSE | <li><h3>Causa</h3></p>{ I_MENSAGEM_CAUSA }</li> | ) &&
                    COND STRING( WHEN I_SYSTEM_RESPONSE IS INITIAL THEN SPACE ELSE | <li><h3>Resposta do Sistema</h3></p>{ I_SYSTEM_RESPONSE }</li> | ) &&
                    COND STRING( WHEN I_WHAT_TO_DO IS INITIAL THEN SPACE ELSE | <li><h3>O que fazer</h3></p>{ I_WHAT_TO_DO }</li> | ) &&
                    COND STRING( WHEN I_SYS_ADMIN IS INITIAL THEN SPACE ELSE | <li><h3>Administrador do Sistema</h3></p>{ I_SYS_ADMIN }</li> | ) &&
                    COND STRING( WHEN I_MENSAGEM_COMPLETA IS INITIAL THEN SPACE ELSE | <li><h3>Mensagem Completa</h3></p>{ I_MENSAGEM_COMPLETA }</li> | ).

  HTML_PAGINA =
    '<!DOCTYPE html>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<html>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    CSS_PAGINA &&
    '<body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<ul>' &&
    HTML_BORY &&
    '</ul>' &&
    '</body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</html>'.

  CALL SCREEN 0100 STARTING AT 05 05.

ENDFUNCTION.
