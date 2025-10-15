FUNCTION zfif_paral_gl_saldo_bsix.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(P_PARCEIRO) TYPE  LIFNR OPTIONAL
*"     VALUE(P_DT_INICIAL) TYPE  BUDAT DEFAULT '19990101'
*"     VALUE(P_DT_POSICAO) TYPE  BUDAT DEFAULT SY-DATUM
*"     VALUE(CONTAS) TYPE  ZCT_EMP_CONTAS OPTIONAL
*"     VALUE(WAERS) TYPE  WAERS OPTIONAL
*"     VALUE(P_GERAR_SOC_PARCEIRA) TYPE  CHAR01 OPTIONAL
*"     VALUE(P_DT_EQ) TYPE  CHAR01 OPTIONAL
*"     VALUE(P_VERIFICA_RZ_ESP) TYPE  CHAR01 DEFAULT SPACE
*"  TABLES
*"      IT_BKPF STRUCTURE  BKPF OPTIONAL
*"      IT_PARTIDAS STRUCTURE  ZDE_FI_GL_PARTIDAS_CLI_FOR OPTIONAL
*"      IT_BSXS STRUCTURE  BSIS OPTIONAL
*"      IT_BSXK STRUCTURE  BSIK OPTIONAL
*"      IT_BSXD STRUCTURE  BSID OPTIONAL
*"      IT_SALDO_CONTAS STRUCTURE  ZDE_FI_GL_SALDO_FAGLFLEXT OPTIONAL
*"      IT_SALDO_PARCEIRO STRUCTURE  ZDE_FI_GL_SALDO_FAGLFLEXT OPTIONAL
*"      IT_SALDO_CTA_PARC STRUCTURE  ZDE_FI_GL_SALDO_FAGLFLEXT2
*"       OPTIONAL
*"----------------------------------------------------------------------

* Chama a Função de Busca Movimentos de FI - BSI/BSA K/D.
  CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
    EXPORTING
      p_parceiro           = p_parceiro
      p_dt_inicial         = p_dt_inicial
      p_dt_posicao         = p_dt_posicao
      contas               = contas
      waers                = waers
      p_gerar_soc_parceira = p_gerar_soc_parceira
      p_dt_eq              = p_dt_eq
      p_verifica_rz_esp    = p_verifica_rz_esp
    TABLES
      it_bkpf              = it_bkpf
      it_partidas          = it_partidas
      it_bsxs              = it_bsxs
      it_bsxk              = it_bsxk
      it_bsxd              = it_bsxd
      it_saldo_contas      = it_saldo_contas
      it_saldo_parceiro    = it_saldo_parceiro
      it_saldo_cta_parc    = it_saldo_cta_parc.

ENDFUNCTION.
