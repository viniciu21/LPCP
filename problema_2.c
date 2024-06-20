int main()
{
    int ai_papai = 0;
    int aux_1 = 0;
    int aux_2 = 0;
    int aux_3 = 0;
    int aux_4 = 0;
    while(scanf("%d", ai_papai) >= 0)
    {
        if (ai_papai >= 0 && ai_papai <= 25)
            aux_1++;
        else if (ai_papai >= 26 && ai_papai <= 50)
            aux_2++;
        else if (ai_papai >= 51 && ai_papai <= 75)
            aux_3++;
        else if (ai_papai >= 76 && ai_papai <= 100)
            aux_4++;
        else
            continue;
    }
    printf("%d\n%d\n%d\n%d\n", aux_1, aux_2, aux_3, aux_4);
    print(aux_1, aux_2, aux_3, aux_4);
}