#include "../conf.h"
#include "../sysdep.h"

#include "../structs.h"
#include "../utils.h"
#include "../db.h"

//void sprintbits(long vektor, char* outstring);

void convert(char* filename)
{
	FILE *fl;
	struct char_file_u player;
	FILE *outfile, *index_file;
	char outname[40], bits[127];
	char buf[200];
	int i;
	int cont = 1;
	struct char_special_data_saved *csds;
	struct player_special_data_saved *psds;
	struct char_ability_data *cad;
	struct char_point_data *cpd;
	struct affected_type *aff;

	if (!(fl = fopen(filename, "r+"))) {
	sprintf(buf, "error opening playerfile (%s)\r\n", filename);
	perror(buf);
		exit(1);
	}

	if(!(index_file = fopen("../lib/pfiles/plr_index", "a"))) {
		printf("error opening index file\r\n");
		exit(1);
	}

	for(;;) {
		fread(&player, sizeof(struct char_file_u), 1, fl);
		if (feof(fl)) {
			printf ("end of file...closing\n");
			fclose(fl);
			fclose(index_file);
			exit(1);
		}

		for (i = 0; (*(bits + i) = LOWER(*(player.name + i))); i++);
		if (isdigit(*bits))
		continue;
		if (!(strchr("abcdefghijklmnopqrstuvwxyz", *bits)))
		continue;
		if (player.char_specials_saved.idnum == 0)
		continue;
		snprintf(outname, sizeof(outname), "../lib/pfiles/%c/%s", *bits, bits);
		printf("writing: %s\n", outname);



		if (!(outfile = fopen(outname, "w"))) {
			printf("error opening output file\r\n");
			cont = 0;
		}
		psds = &(player.player_specials_saved);
		fseek(index_file, -1, SEEK_END);
		fprintf(index_file, "%ld %s %d 0 %ld %d %d\n",
				(long)player.char_specials_saved.idnum, bits, player.level,
				player.last_logon, psds->clan ? psds->clan : -1, psds->clan_rank);
		if (cont)
		{
		if(player.name)
			fprintf(outfile, "Name: %s\n", player.name);
		if(player.pwd)
			fprintf(outfile, "Pass: %s\n", player.pwd);
		if(player.title)
			fprintf(outfile, "Titl: %s\n", player.title);
		if(player.description && *player.description)
		fprintf(outfile, "Desc:\n%s~\n", player.description);
		fprintf(outfile, "Sex : %d\n", (int)player.sex);
		fprintf(outfile, "Clas: %d\n", (int)player.chclass);
		fprintf(outfile, "Race: %d\n", (int)player.race);
		fprintf(outfile, "Levl: %d\n", (int)player.level);
		fprintf(outfile, "Home: %d\n", (int)player.hometown);
		fprintf(outfile, "Brth: %d\n", (int)player.birth);
		fprintf(outfile, "Plyd: %d\n", (int)player.played);
		fprintf(outfile, "Last: %d\n", (int)player.last_logon);
		fprintf(outfile, "Host: %s\n", player.host);
		fprintf(outfile, "Hite: %d\n", (int)player.height);
		fprintf(outfile, "Wate: %d\n", (int)player.weight);

		csds = &(player.char_specials_saved);
		fprintf(outfile, "Alin: %d\n", csds->alignment);
		fprintf(outfile, "Id  : %ld\n", csds->idnum);
		fprintf(outfile, "Act : %u %u %u %u\n", csds->act[0],
				csds->act[1], csds->act[2], csds->act[3]);
		fprintf(outfile, "Aff : %u %u %u %u\n", csds->affected_by[0],
				csds->affected_by[1], csds->affected_by[2],
				csds->affected_by[3]);
		fprintf(outfile, "Thr1: %d\n", csds->apply_saving_throw[0]);
		fprintf(outfile, "Thr2: %d\n", csds->apply_saving_throw[1]);
		fprintf(outfile, "Thr3: %d\n", csds->apply_saving_throw[2]);
		fprintf(outfile, "Thr4: %d\n", csds->apply_saving_throw[3]);
		fprintf(outfile, "Thr5: %d\n", csds->apply_saving_throw[4]);

		//psds = &(player.player_specials_saved);
		if(player.level < LVL_IMMORT) {
			fprintf(outfile, "Skil:\n");
			for(i = 1; i < MAX_SKILLS; i++) {

				if(psds->skills[i])
					fprintf(outfile, "%d %d 0 0\n",i, (int)psds->skills[i]);
			}
			fprintf(outfile, "0 0 0 0\n");
		}
		fprintf(outfile, "Flag: %ld\n", psds->cmd);
		fprintf(outfile, "Wimp: %d\n", psds->wimp_level);
		fprintf(outfile, "Frez: %d\n", (int)psds->freeze_level);
		fprintf(outfile, "Invs: %d\n", (int)psds->invis_level);
		fprintf(outfile, "Room: %d\n", (int)psds->load_room);
		fprintf(outfile, "Pref: %u %u %u %u\n", psds->pref[0],
				psds->pref[1], psds->pref[2], psds->pref[3]);
		fprintf(outfile, "Badp: %d\n", (int)psds->bad_pws);
		fprintf(outfile, "Hung: %d\n", (int)psds->conditions[0]);
		fprintf(outfile, "Thir: %d\n", (int)psds->conditions[1]);
		fprintf(outfile, "Drnk: %d\n", (int)psds->conditions[2]);
		fprintf(outfile, "Lern: %d\n", (int)psds->spells_to_learn);

		cad = &(player.abilities);
		fprintf(outfile, "Str : %d/%d\n", cad->str, cad->str_add);
		fprintf(outfile, "Int : %d\n", cad->intel);
		fprintf(outfile, "Wis : %d\n", cad->wis);
		fprintf(outfile, "Dex : %d\n", cad->dex);
		fprintf(outfile, "Con : %d\n", cad->con);
		fprintf(outfile, "Cha : %d\n", cad->cha);

		cpd = &(player.points);
		fprintf(outfile, "Hit : %d/%d\n", cpd->hit, cpd->max_hit);
		fprintf(outfile, "Mana: %d/%d\n", cpd->mana, cpd->max_mana);
		fprintf(outfile, "Move: %d/%d\n", cpd->move, cpd->max_move);
		fprintf(outfile, "Ac  : %d\n", cpd->armor);
		fprintf(outfile, "Gold: %ld\n", (long)cpd->gold);
		fprintf(outfile, "Bank: %ld\n", (long)cpd->bank_gold);
		fprintf(outfile, "Exp : %ld\n", (long)cpd->exp);
		fprintf(outfile, "Hrol: %d\n", cpd->hitroll);
		fprintf(outfile, "Drol: %d\n", cpd->damroll);

		fprintf(outfile, "Rem1: %d\n", player.was_class);
		fprintf(outfile, "Rem2: %d\n", player.was_class1);
		fprintf(outfile, "Rem3: %d\n", player.was_class2);
		fprintf(outfile, "Roma: %d\n", player.romance);
		fprintf(outfile, "Preg: %d\n", player.ticks_left);
		if (player.partner)
			fprintf(outfile, "Part: %s\n", player.partner);
		fprintf(outfile, "Clan: %d\n", psds->clan);
		fprintf(outfile, "ClRk: %d\n", psds->clan_rank);
		fprintf(outfile, "BraT: %d\n", psds->brass_tokens);
		fprintf(outfile, "BroT: %d\n", psds->bronze_tokens);
		fprintf(outfile, "SilT: %d\n", psds->silver_tokens);
		fprintf(outfile, "GolT: %d\n", psds->gold_tokens);
		fprintf(outfile, "RipC: %d\n", psds->rip_cnt);
		fprintf(outfile, "KilC: %d\n", psds->kill_cnt);
		fprintf(outfile, "DTC : %d\n", psds->dt_cnt);
		fprintf(outfile, "BetO: %d\n", psds->betted_on);
		fprintf(outfile, "Tir1: %d\n", (int)psds->tier);
		fprintf(outfile, "Tir2: %d\n", (int)psds->tier1);
		fprintf(outfile, "Tir3: %d\n", (int)psds->tier2);
		fprintf(outfile, "Tir4: %d\n", (int)psds->tier3);

		fprintf(outfile, "Affs:\n");
		for(i = 0; i < MAX_AFFECT; i++) {
			aff = &(player.affected[i]);
			if(aff->type)
				fprintf(outfile, "%d %d %d %d %d\n", aff->type, aff->duration,
						aff->modifier, aff->location, (int)aff->bitvector);
		}
		fprintf(outfile, "0 0 0 0 0\n");

		fclose(outfile);
		}
		cont = 1;
	}
}

int main(int argc, char **argv)
{
	if (argc != 2)
		printf("Usage: %s playerfile-name\n", argv[0]);
	else
		convert(argv[1]);

	return 0;
}
