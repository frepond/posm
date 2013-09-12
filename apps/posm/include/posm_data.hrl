-record(posm_kv,  {key, 
				   value
				  }).

%% tables bank and account
-record(posm_bank, {cubco    :: string(),	 % Identificador del Banco
			        nombco   :: string(),	 % Nombre del Banco
			        accounts ::list()        % Cuentas del banco [{cubcta, nro_cta, nro_cbu}]
			  	   }).