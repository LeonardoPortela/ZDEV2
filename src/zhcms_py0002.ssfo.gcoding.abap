
WRITE wa_dados-salariodecalculo TO lc_salario_fixo_txt.
CONDENSE lc_salario_fixo_txt NO-GAPS.

CLEAR: lc_logo.
CLEAR: lc_moedas.

*0001	AMAGGI EXP.E IMP.LTDA	USAR  DA AMAGGI
*0005	ANDRÉ MAGGI PARTICIPAÇÕES	USAR  DA AMAGGI
*0010	HERMASA NAVEGAÇÃO DA AMAZ	USAR  DA AMAGGI
*0015	AGROPECUÁRIA MAGGI LTDA	USAR  DA AMAGGI
*0022	MAGGI ENERGIA S/A	USAR  DA AMAGGI
*0023	CIDEZAL AGRÍCOLA LTDA	USAR  DA AMAGGI
*0025	ILHA COMPRIDA ENERGIA S/A	USAR  DA AMAGGI
*0026	SEGREDO ENERGIA S/A	USAR  DA AMAGGI
*0027 DIVISA ENERGIA S/A  USAR  DA AMAGGI
*0028	ARUANA COMERC.ENERGIA LTD	USAR  DA AMAGGI
*0032 AMAGGI PECUARIA LTDA  USAR  DA AMAGGI
*0040 AMAGGI COM.SERVICOS LTDA  USAR  DA AMAGGI

*0043	AMAGGI SERV.FINANCEIROS	FINANCEIRA

*0044 CIA AGRIC.PARECIS-CIAPAR  ????

*0035	AMAGGI LD ZEN-NOH GRAO SA	ALZ
*0038 AMAGGI & LD TERM.PORT.SA  ALZ

*0039	NAVEG. UN.TAPAJOS S/A	USAR DA UNITAPAJOS
*0041 TERMINAL FRONT.N.LOG.S/A  USAR DA UNITAPAJOS

IF wa_dados-bukrs EQ '0039'. lc_logo = 'U'. ENDIF.
IF wa_dados-bukrs EQ '0041'. lc_logo = 'U'. ENDIF.

IF wa_dados-bukrs EQ '0035'. lc_logo = 'Z' .ENDIF.
IF wa_dados-bukrs EQ '0038'. lc_logo = 'Z'. ENDIF.


IF wa_dados-bukrs EQ '0001'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0005'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0010'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0015'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0022'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0023'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0025'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0026'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0027'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0028'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0032'. lc_logo = 'A'. ENDIF.
IF wa_dados-bukrs EQ '0040'. lc_logo = 'A'. ENDIF.

IF wa_dados-bukrs EQ '0043'. lc_logo = 'F'. ENDIF.

IF lc_logo IS INITIAL.
  lc_logo = 'A'.
ENDIF.

IF  wa_dados-bukrs EQ '0035'.  lc_moedas = 'X'.  ENDIF.
IF  wa_dados-bukrs EQ '0038'.  lc_moedas = 'X'.  ENDIF.
IF  wa_dados-bukrs EQ '0039'.  lc_moedas = 'X'.  ENDIF.
IF  wa_dados-bukrs EQ '0041'.  lc_moedas = 'X'.  ENDIF.
IF  wa_dados-bukrs EQ '0043'.  lc_moedas = 'X'.  ENDIF.
