% Plots the sparsity patterns of the  Jacobian and the Hessian

%%%%% JACOBIAN
figure(1);

J = zeros(NVAR,NVAR);
for k=1:LU_NONZERO
  J( LU_IROW(k), LU_ICOL(k) ) = 1;
end
spy(J);
TK = floor( linspace(1,NVAR,5) );
set(gca,'XTick',TK,'YTick',TK,'FontSize',14,'LineWidth',2);

%%%%% HESSIAN
figure(2);

H = zeros(NVAR,NVAR,NVAR);
for k=1:NHESS
  H( IHESS_I(k), IHESS_J(k), IHESS_K(k) ) = 1;
  H( IHESS_I(k), IHESS_K(k), IHESS_J(k) ) = 1;
end

M = ceil( sqrt(NVAR) );

for i=1:M
  for j=1:M
    k = M*(i-1)+j;
    if ( k <= NVAR )
      subplot(M,M,k);
      hold on
      G = reshape( H(k,1:NVAR,1:NVAR), NVAR, NVAR );
      % spy(G); figure;hold on
      for iH=1:NVAR; for jH=1:NVAR
         if( G(iH, jH) )
	    plot(iH,NVAR+1-jH,'.'); 
	 end;
      end; end
      text(NVAR/6,NVAR*0.6,int2str(k))
      set(gca,'XLim',[1,NVAR],'YLim',[1,NVAR]);
      set(gca,'XTick',[],'YTick',[]);
      axis('square');
      box on     
      hold off;
    end
  end
end    
