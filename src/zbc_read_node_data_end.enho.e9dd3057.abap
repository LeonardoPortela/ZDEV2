"Name: \PR:SAPLSX01\FO:READ_NODE_DATA\SE:END\EI
ENHANCEMENT 0 ZBC_READ_NODE_DATA_END.
* sendgrid stmp api requirement : user name apikey in lower case.
  IF ls_node-mail_host EQ 'smtp.sendgrid.net'.
    TRANSLATE es_node-smtpuser TO LOWER CASE.
  ENDIF.
ENDENHANCEMENT.
