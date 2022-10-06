#include<stdio.h>
#include<conio.h>
#include<math.h>
#include<stdlib.h>
/* ---------------------------------------- Fungsi dan Prosedur yang digunakan ------------------------------------------------------- */
void Solution(int baris, int kolom, float matrix[baris][kolom+1] );
int validation(int baris, int kolom, float matrix[baris][kolom+1] );
float determinant(float a[25][25],float k);
void cofactor(float num[25][25],float f);
void transpose(float num[25][25],float fac[25][25],float r);
void operasi();
void multiply(int, int, int [][10], int, int, int [][10], int [][10]);
void display(int, int, int[][10]);
void perkalian();
void InputMatrix(int jml_variable, int jml_persamaan, int new_persamaan, float matrix[new_persamaan][jml_variable+1] );
void DisplayMatriks(int new_persamaan, int jml_variable, float matrix[new_persamaan][jml_variable+1]);
void OBE (int row, int col, float mat[row][col+1]);
int SegitigaTes(int m, int n, float A[25][25]);

void InputMatrix(int jml_variable, int jml_persamaan, int new_persamaan, float matrix[new_persamaan][jml_variable+1] ){
	int i,j;
	for(i=0;i<new_persamaan;i++){
		if(i<jml_persamaan){
			printf("persamaan %d\n",i+1);
		}
		for(int j=0;j<=jml_variable;j++){
			if(i>=jml_persamaan){
				matrix[i][j]=0;
			}else{
				if(j==jml_variable){
					printf("hasil persamaan = ");
				}else{
					printf("x[%d] = ",j+1);
				}	
				scanf("%f",&matrix[i][j]);
				fflush(stdin);
			}	
		}
		printf("\n");
	}
}

void DisplayMatriks(int new_persamaan, int jml_variable, float matrix[25][jml_variable+1]){
	for(int i=0;i<new_persamaan;i++){
		for(int j=0;j<=jml_variable;j++){
			printf("%4.2g ",matrix[i][j]);
		}
		printf("\n");
	}
}

void DisplaySPL(int new_persamaan, int jml_variable, float matrix[new_persamaan][jml_variable+1]){
	for(int i=0;i<new_persamaan;i++){
		for(int j=0;j<=jml_variable;j++){
			if(j>0){
				if(j==jml_variable){
					printf(" = ");
				}else if(matrix[i][j]>=0){
					printf(" + ");
				}
			}			
			printf("%4.2g",matrix[i][j]);
			if(j!=jml_variable){
				printf(" x[%d] ",j+1);
			}
		}
		printf("\n");
	}
}

int isUpperTriangularMatrix(int m, float mat[25][25])
{
	int i, j;
//	printMatrix(baris, baris,mat);
    for (i = 1; i < m; i++){
        for (j = 0; j < i; j++){
            if (mat[i][j] != 0)
                return 0;
            }
	}
    return 1;
}

void OBE (int row, int col, float mat[row][col+1])
{
	// Program Logic
	int k=0,j,i,a,b;
	float tem;
//	for(k=0;k<row;k++)
	while(1)
	{
		
		if( (mat[k][k]) != 1)
		{
			float temp = mat[k][k];
			if(temp == 0){
				if(k == 0){
					printf("Tidak Dapat Dilakukan OBE");
					return;
				}
				continue; // Avoiding division by zero
			}
		

			for(j=0;j<=col;j++)
			{
				mat[k][j] = ( (mat[k][j]) / temp );
			}
		}
		
		for(i=k+1;i<row;i++)
		{
			tem = mat[i][k];
			for(j=k;j<=col;j++)
				{
					
					mat[i][j] = mat[i][j] - ( mat[k][j] * tem );
				}
		
		}

		// Printing Each Step
		printf("\n**************************\n");
		if(k==row-1)
			printf("Matriks Eselon Baris : \n\n");
		else
			printf("Step %d\n\n",k+1);
		for(a=0;a<row;a++)
		{
			for(b=0;b<=col;b++)
			{
				if(mat[a][b] == -0)
					mat[a][b] = 0; // Simply converting'-0' into '0'
				printf("%.1f\t",mat[a][b]);
			}
			printf("\n");
		}
		
		if(k==row-1)
			break;
		
		k++;
	}
	Solution(row, col, mat);
	
}

int SegitigaTes(int m, int n, float A[25][25]){
	int row,col,isUpper;
	isUpper = 1;
    for(row=0; row<m; row++)
    {
        for(col=0; col<n; col++)
        {
            /*
             * If elements below the main diagonal (col<row)
             * is not equal to zero then it is not upper triangular matrix
             */
            if(col<row && A[row][col]!=0)
            {
                isUpper = 0;
                return isUpper;
            }
        }
    }
    return isUpper;
}
void menu(){
	printf("============================================================================================\n");
	printf("                                         Kalkulator Matriks                                   \n");
	printf("============================================================================================\n\n");
	printf("1. Operasi Aritmatika Matriks\n");
	printf("2. Determinan dan Invers menggunakan kofaktor\n");
	printf("3. Penyelesaian SPL dengan OBE\n");
}
int main(){
	int pilih, lanjut;
	
	do{
		menu();
		printf("Silahkan Pilih [1/2/3] :");
		scanf("%d", &pilih);
		fflush(stdin);
		
		switch (pilih){
			case 1:
				printf("\n\n========================================== OPERASI MATRIKS ====================================================\n");
				operasi();
				break;
			case 2:
			{
				float a[25][25],k,d;
  				int i,j;
				printf("\n\n======================================== DETERMINAN & INVERS ===================================================\n");
				printf("\nMasukkan ordo matriks : ");
  				scanf("%f",&k);
  				fflush(stdin);
  				printf("Masukkan elemen dari matriks %.0fX%.0f \n",k,k);
  				for (i=0;i<k;i++)
   				{
     				for (j=0;j<k;j++)
       				{
        			printf("A(%d,%d): ",i+1,j+1);
        			scanf("%f",&a[i][j]);
        			fflush(stdin);
        			}
    			}
  				d=determinant(a,k);
 				printf("Determinan = %.2f",d);
  				if (d==0)
   				printf("\nInvers matriks tidak temukan\n");
 				else
  				cofactor(a,k);
  			}
  				break;
  			case 3:
  				{
  					int jml_variable, jml_persamaan, tambahan=0, new_persamaan;
					printf("\n\n============================================ SPL dengan OBE ======================================================\n");
					printf("Catatan:\n");
					printf("Jumlah Variable ini berlaku untuk semua persamaan.\nJika ada variabel yang sebenarnya tidak ada maka beri nilai 0 saja\n");
					printf("\nJumlah variable: ");
					scanf("%d",&jml_variable);
					fflush(stdin);
					printf("Jumlah persamaan: ");
					scanf("%d",&jml_persamaan);
					fflush(stdin);
					if(jml_persamaan<jml_variable){
						tambahan=jml_variable-jml_persamaan;
					}
					new_persamaan=jml_persamaan+tambahan;
					float matrix[new_persamaan][jml_variable+1];
					InputMatrix(jml_variable, jml_persamaan, new_persamaan, matrix);
					printf("Persamaan yang dihasilkan: \n");
					DisplaySPL(new_persamaan, jml_variable, matrix);
					printf("\n\nMatriks dari hasil persamaan di atas: \n");
					DisplayMatriks(new_persamaan, jml_variable, matrix);
					printf("\nPenyelesaian dengan OBE\n");
					OBE(new_persamaan, jml_variable, matrix);
				}
				break;
			default:
				printf("Input tidak valid!!!\n");
		}
		fflush(stdin);
		printf("\nApakah ingin menggunakan kalkulator matriks lagi [Y/N] : ");
		scanf("%c", &lanjut);
		fflush(stdin);
		system("cls");
	}while(lanjut == 'Y' || lanjut == 'y' );
	printf("\n============================================= TERIMA KASIH =========================================================\n");
		
	
}

void Solution(int baris, int kolom, float matrix[baris][kolom+1] ){
//now performing back substitution
int s, i, j;
float hasil[kolom];
if (validation(baris, kolom,matrix) == 0){
	printf("\nPersamaan Memiliki Banyak Penyelesaian !!!");
}else{
	for (i = baris - 1; i >= 0; i--)
    {
        s = 0;
        for (j = i + 1; j < baris; j++)
        {
            s += matrix[i][j] * hasil[j];
        }
        hasil[i] = (matrix[i][baris] - s) / matrix[i][i];
    }

    //now printing the result
    printf("\n Solusi Dari Persamaan di atas : \n");
    for (i = 0; i < baris; i++)
        printf("\n x[%d] = %.2f ", i + 1, hasil[i]);
	}
}

int validation(int baris, int kolom, float matrix[baris][kolom+1] ){
	int i, j, count;
	for (i = baris-1; i >= 0; i--)
    {
    	count=0;
        for (j = 0; j < kolom; j++)
        {
            if(matrix[i][j] == 0){
            	count++;
			}
        }
        if(count == kolom){
        	return 0;
		}  
    }
    return 1;
}

float determinant(float a[25][25],float k)
{
  float s=1,det=0,b[25][25];
  int i,j,m,n,c;
  if (k==1)
    {
     return (a[0][0]);
    }
  else
    {
     det=0;
     for (c=0;c<k;c++)
       {
        m=0;
        n=0;
        for (i=0;i<k;i++)
          {
            for (j=0;j<k;j++)
              {
                b[i][j]=0;
                if (i != 0 && j != c)
                 {
                   b[m][n]=a[i][j];
                   if (n<(k-2))
                    n++;
                   else
                    {
                     n=0;
                     m++;
                     }
                      }
               }
             }
          det=det + s * (a[0][c] * determinant(b,k-1));
          s=-1 * s;
          }
    }

    return (det);
}

void cofactor(float num[25][25],float f)
{
 float b[25][25],fac[25][25];
 int p,q,m,n,i,j;
 for (q=0;q<f;q++)
 {
   for (p=0;p<f;p++)
    {
     m=0;
     n=0;
     for (i=0;i<f;i++)
     {
       for (j=0;j<f;j++)
        {
          if (i != q && j != p)
          {
            b[m][n]=num[i][j];
            if (n<(f-2))
             n++;
            else
             {
               n=0;
               m++;
               }
            }
        }
      }
      fac[q][p]=pow(-1,q + p) * determinant(b,f-1);
    }
  }
  transpose(num,fac,f);
}
/*Finding transpose of matrix*/
void transpose(float num[25][25],float fac[25][25],float r)
{
  int i,j;
  float b[25][25],inverse[25][25],d;

  for (i=0;i<r;i++)
    {
     for (j=0;j<r;j++)
       {
         b[i][j]=fac[j][i];
        }
    }
  d=determinant(num,r);
  for (i=0;i<r;i++)
    {
     for (j=0;j<r;j++)
       {
        inverse[i][j]=b[i][j] / d;
        }
    }
   printf("\nInvers dari matrix : \n");

   for (i=0;i<r;i++)
    {
     for (j=0;j<r;j++)
       {
         printf("\t%.1f",inverse[i][j]);
        }
    printf("\n");
     }
}

void operasi(){
int i, j, m, n, matriks1[10][10], matriks2[10][10], hasil[10][10];
  printf("Masukkan jumlah baris matriks: ");
  scanf("%d", &m);
  fflush(stdin);
  printf("Masukkan jumlah kolom matriks: ");
  scanf("%d", &n);
  fflush(stdin);
  printf("\nMasukkan elemen matrix pertama: \n");
  for (i = 0; i < m; i++)
  {
    for (j = 0; j < n; j++)
    {
      scanf("%d", &matriks1[i][j]);
    }
  }
  printf("\nMasukkan elemen matrix kedua: \n");
  for (i = 0; i < m; i++)
  {
    for (j = 0; j < n; j++)
    {
      scanf("%d", &matriks2[i][j]);
      
    }
  }
  
  // PENJUMLAHAN
  printf("\nhasil penjumlahan matrix: \n");
  for (i = 0; i < m; i++)
  {
    for (j = 0; j < n; j++)
    {
      hasil[i][j] = matriks1[i][j] + matriks2[i][j];
      printf("%d \t", hasil[i][j]);
    }
    printf("\n");
  }
  
  // PENGURANGAN
  printf("Hasil pengurangan matriks: \n");
  for (i = 0; i < m; i++)
  {
    for (j = 0; j < n; j++)
    {
      hasil[i][j] = matriks1[i][j] - matriks2[i][j];
      printf("%d \t", hasil[i][j]);
    }
    printf("\n");
  }
  
  printf("\nPERKALIAN\n");
  perkalian();
}

void multiply (int m1, int n1, int a[10][10], int m2, int n2, int b[10][10], int c[10][10])
{
    static int i = 0, j = 0, k = 0;
 
    if (i >= m1)
    {
        return;
    }
    else if (i < m1)
    {
        if (j < n2)
        {
            if (k < n1)
            {
                c[i][j] += a[i][k] * b[k][j];
                k++;
                multiply(m1, n1, a, m2, n2, b, c);
            }
            k = 0;
            j++;
            multiply(m1, n1, a, m2, n2, b, c);
        }
        j = 0;
        i++;
        multiply(m1, n1, a, m2, n2, b, c);
    }
}
 
void display(int m1, int n2, int c[10][10])
{
    int i, j;
 
    for (i = 0; i < m1; i++)
    {
        for (j = 0; j < n2; j++)
        {
            printf("%4d  ", c[i][j]);
        }
        printf("\n");
    }
}

void perkalian(){
	int a[10][10], b[10][10], c[10][10] = {0};
    int m1, n1, m2, n2, i, j, k;
 
    printf("Masukkan baris dan kolom matriks A(m n): ");
    scanf("%d %d", &m1, &n1);
    fflush(stdin);
    printf("Masukkan baris dan kolom matriks B(m n): ");
    scanf("%d %d", &m2, &n2);
    fflush(stdin);
    if (n1 != m2)
    {
        printf("Matriks tidak dapat dikalikan.\n");
    }
    else
    {
        printf("Masukkan elements untuk Matrix A:\n");
        for (i = 0; i < m1; i++)
        for (j = 0; j < n1; j++)
        {
            scanf("%d", &a[i][j]);
           
        }
        printf("\nMasukkan elements untuk Matrix B:\n");
        for (i = 0; i < m2; i++)
        for (j = 0; j < n2; j++)
        {
            scanf("%d", &b[i][j]);
        }
        multiply(m1, n1, a, m2, n2, b, c);
	    printf("Matrix A:\n");
	    display(m1,n1,a);
        printf("Matrix A:\n");
    display(m1,n1,a);
    printf("Matrix B:\n");
    display(m2,n2,b);
    printf("Hasil Perkalian:\n");
    display(m1, n2, c);
    

}
}  