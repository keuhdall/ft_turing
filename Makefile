#* ************************************************************************** *#
#*                                                                            *#
#*                                                        :::      ::::::::   *#
#*   Makefile                                           :+:      :+:    :+:   *#
#*                                                    +:+ +:+         +:+     *#
#*   By: lmarques <lmarques@student.42.fr>          +#+  +:+       +#+        *#
#*                                                +#+#+#+#+#+   +#+           *#
#*   Created: 2017/11/19 05:25:46 by lmarques          #+#    #+#             *#
#*   Updated: 2018/03/08 07:48:30 by lmarques         ###   ########.fr       *#
#*                                                                            *#
#* ************************************************************************** *#

NAME=ft_turing

SRC_PATH=./src

all: $(NAME)

$(NAME):
	@ghc --make -i:src src/main.hs -o $(NAME)

clean:
	@find . -type f -name "*.o" -delete
	@find . -type f -name "*.hi" -delete

fclean: clean
	@rm -fv $(NAME)

re:fclean all

.PHONY: all, clean, fclean, re
