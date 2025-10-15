*----------------------------------------------------------------------*
***INCLUDE ZHCMR_PA0013_AD_CORPO_EMAILF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CORPO_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_ZHCMT0007_AD_TX_EMAIL  text
*      -->P_WA_ZHCMT0007_AD  text
*      -->P_WA_ZHCMT0007  text
*----------------------------------------------------------------------*
FORM CORPO_EMAIL  USING    P_ZHCMT0007_AD TYPE ZHCMT0007_AD
                           P_ZHCMT0007    TYPE ZHCMT0007
                  CHANGING TX_EMAIL TYPE STRING.

  CLEAR: TX_EMAIL.
  TX_EMAIL = '<!DOCTYPE html>' &&
             '<html>' &&
             '<head>' &&
             '<style>' &&
             'table, th, td {' &&
             '    border: 1px solid black; border-collapse: collapse; ' &&
             '}' &&
             '</style>' &&
             '</head>' &&
             '<body>' &&
             '<h2>SAP HCM</h2>' &&
             '<p>Inclusão de Usuário no Active Directory</p>' &&
             '<table style="width:100%">' &&
             '<tr><th>Matrícula</th><th>CPF</th><th>Usuário</th><th>Senha</th></tr>' &&
             '<tr><td>' && P_ZHCMT0007-PERNR && '</td><td>' && P_ZHCMT0007-CPF_NR && '</td><td>' && P_ZHCMT0007-SAMACCOUNTNAME && '</td><td>' && P_ZHCMT0007-SENHA_INICIAL && '</td></tr>' &&
             '</table>' &&
             '</body>' &&
             '</html>'.

ENDFORM.
